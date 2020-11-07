module Main where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ARF
import Affjax.StatusCode (StatusCode(..))
import Data.Array (filter, find, length, null)
import Data.Codec.Argonaut (JsonCodec, decode, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Buffer (fromArrayBuffer, toString)
import Node.ChildProcess (defaultExecOptions, defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, writeFile)
import Web.File.FileReader (result)

parentFolder :: String
parentFolder = "./files"

originalFile :: String
originalFile = "packages-original.dhall"

fixedFile :: String
fixedFile = "packages-fixed.dhall"

main :: Effect Unit
main = launchAff_ do
  whenM (not <$> exists parentFolder) do
    mkdir parentFolder
  let baseUrl = "https://api.github.com/repos/purescript/package-sets/releases"
  result <- Ajax.get ARF.json baseUrl
  releases <- recursivelyFetchReleases baseUrl [] 1
  let targetReleases = filter (\r -> not $ null r.assets) releases
  log $ "Found " <> show (length targetReleases) <> " releases."
  for_ targetReleases \releaseInfo -> do
    let tagName = releaseInfo.name
    case find (eq "packages.dhall" <<< _.name) releaseInfo.assets of
      Nothing -> do
        log $ "Skipping release: " <> tagName
        log ""
      Just asset -> do
        log $ "Downloading file from release: " <> tagName
        downloadPackagesDhallFile tagName asset.browser_download_url
        log ""
  log "Finished."

type ReleaseInfo =
  { name :: String
  , assets :: Array
    { name :: String
    , browser_download_url :: String
    , id :: Int
    }
  }

releaseCodec :: JsonCodec ReleaseInfo
releaseCodec = CAR.object "Release"
  { name: CA.string
  , assets: CA.array $
    CAR.object "assets"
      { name: CA.string
      , browser_download_url: CA.string
      , id: CA.int
      }
  }


recursivelyFetchReleases :: String -> Array ReleaseInfo -> Int -> Aff (Array ReleaseInfo)
recursivelyFetchReleases baseUrl accumulator page = do
  pageNResult <- fetchNextPageOfReleases baseUrl page
  case pageNResult of
    Nothing -> pure accumulator
    Just arr -> recursivelyFetchReleases baseUrl (accumulator <> arr) (page + 1)

fetchNextPageOfReleases :: String -> Int -> Aff (Maybe (Array ReleaseInfo))
fetchNextPageOfReleases baseUrl page = do
  -- API doc: https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-releases
  let url = baseUrl <> "?per_page=100&page=" <> show page

  result <- Ajax.get ARF.json url
  case result of
    Left err -> do
      throwError $ error $ Ajax.printError err
    Right response
      | response.status /= StatusCode 200 -> do
          throwError $ error $ show response.status <> " " <> response.statusText
      | otherwise -> case decode (CA.array releaseCodec) response.body of
          Left e -> do
            throwError $ error $ printJsonDecodeError e
          Right releases
            | null releases ->
                pure Nothing
            | otherwise -> do
                pure $ Just releases

downloadPackagesDhallFile :: String -> String -> Aff Unit
downloadPackagesDhallFile tagName browser_download_url = do
  -- API doc: https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#get-a-release-asset
  log $ "browser_download_url: " <> browser_download_url
  result <- Ajax.get ARF.arrayBuffer browser_download_url
  case result of
    Left err -> do
      throwError $ error $ Ajax.printError err
    Right response
      | response.status /= StatusCode 200 -> do
          log "Failed to get package file"
          -- log response.body
          throwError $ error $ show response.status <> " " <> response.statusText
      | otherwise -> do
        buffer <- liftEffect $ fromArrayBuffer response.body
        let
          tagFolder = parentFolder <> "/" <> tagName
          oldFile = tagFolder <> "/" <> originalFile
          newFile = tagFolder <> "/" <> fixedFile
        whenM (not <$> exists tagFolder) do
          mkdir tagFolder

        writeFile oldFile buffer
        hashesAreSame <- liftEffect do
          originalBuffer <- execSync ("./dhall-1.32.0 hash --file " <> oldFile) defaultExecSyncOptions
          void $ execSync ("cp " <> oldFile <> " " <> newFile) defaultExecSyncOptions
          void $ execSync ("sed -i 's/ assert /`assert`/' " <> newFile) defaultExecSyncOptions
          newBuffer <- execSync ("./dhall-1.36.0 hash --file " <> newFile) defaultExecSyncOptions
          originalHash <- toString UTF8 originalBuffer
          newHash <- toString UTF8 newBuffer
          log $ "Old hash: " <> originalHash
          log $ "New hash: " <> newHash
          pure $ originalHash == newHash
        if hashesAreSame then do
          log "Hashes are good. Here we would upload to GitHub."
        else do
          throwError $ error $ "Different hashes. Aborting"
