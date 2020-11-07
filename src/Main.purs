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
import Node.Buffer (Buffer, fromArrayBuffer, toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, writeFile)

{-
Before running this program, you need 2 files:
- dhall-1.32.0 - the `dhall` program's file at version 1.32.0
- dhall-1.36.0 - the `dhall` program's file at version 1.36.0

When you run this program, the fixed files will not overwrite the corresponding
file for that tag's `packages.dhall` file. It will show that the code works
correctly.

If you want to use this program to upload and overwrite the current
`packages.dhall` file with their updated version, the following must be true
- `gh auth status` will show that you are logged in
- `unsafe_Upload_Fixed_Packages_Dhall_File` was changed to true

-}


-- If this value is true and you run this program, it will overwrite
-- the packages.dhall file in each release with its fixed version.
unsafe_Upload_Fixed_Packages_Dhall_File :: Boolean
unsafe_Upload_Fixed_Packages_Dhall_File = false

fileFolder :: String
fileFolder = "./files"

main :: Effect Unit
main = launchAff_ do
  whenM (not <$> exists fileFolder) do
    mkdir fileFolder

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
        let assetInfo =
              { tagName: releaseInfo.name
              , browser_download_url: asset.browser_download_url
              }
        buffer <- downloadPackagesDhallFile assetInfo
        fixFileAndUploadResult assetInfo buffer
        log ""
  log "Finished."

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

downloadPackagesDhallFile :: AssetInfo -> Aff Buffer
downloadPackagesDhallFile rec = do
  -- API doc: https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#get-a-release-asset
  log $ "browser_download_url: " <> rec.browser_download_url
  result <- Ajax.get ARF.arrayBuffer rec.browser_download_url
  case result of
    Left err -> do
      throwError $ error $ Ajax.printError err
    Right response
      | response.status /= StatusCode 200 -> do
          log "Failed to get package file"
          throwError $ error $ show response.status <> " " <> response.statusText
      | otherwise -> do
        liftEffect $ fromArrayBuffer response.body

fixFileAndUploadResult :: AssetInfo -> Buffer -> Aff Unit
fixFileAndUploadResult rec buffer = do
  let
    tagFolder = fileFolder <> "/" <> rec.tagName
    oldFile = tagFolder <> "/" <> "packages-original.dhall"
    newFile = tagFolder <> "/" <> "packages-fixed.dhall"
    uploadFile = tagFolder <> "/" <> "packages.dhall"
  whenM (not <$> exists tagFolder) do
    mkdir tagFolder

  writeFile oldFile buffer
  liftEffect do
    let deso = defaultExecSyncOptions
    void $ execSync ("cp " <> oldFile <> " " <> newFile) deso
    void $ execSync ("sed -i 's/ assert /`assert`/' " <> newFile) deso

    originalBuffer <- execSync ("./dhall-1.32.0 hash --file " <> oldFile) deso
    originalHash <- toString UTF8 originalBuffer
    log $ "Original: " <> originalHash
    newBuffer <- execSync ("./dhall-1.36.0 hash --file " <> newFile) deso
    newHash <- toString UTF8 newBuffer
    log $ "Fixed:    " <> newHash

    if (originalHash == newHash) then do
      log "Hashes match"
      if unsafe_Upload_Fixed_Packages_Dhall_File then do
        void $ execSync ("mv " <> newFile <> " " <> uploadFile) defaultExecSyncOptions
        void $ execSync ("gh release upload " <> rec.tagName <> " " <> uploadFile <> " --clobber --repo purescript/package-sets") defaultExecSyncOptions
      else do
        log "While hashes match, we aren't uploading this."
    else do
      throwError $ error $ "Different hashes. Aborting"

-- codec info

type ReleaseInfo =
  { name :: String -- the tag name
  , assets :: Array
    { name :: String -- the file name
    , browser_download_url :: String -- url for downloading it
    }
  }

type AssetInfo =
  { tagName :: String
  , browser_download_url :: String
  }

releaseCodec :: JsonCodec ReleaseInfo
releaseCodec = CAR.object "Release"
  { name: CA.string
  , assets: CA.array $
    CAR.object "assets"
      { name: CA.string
      , browser_download_url: CA.string
      }
  }
