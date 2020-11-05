module Main where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ARF
import Affjax.StatusCode (StatusCode(..))
import Data.Array (filter, null)
import Data.Codec.Argonaut (JsonCodec, decode, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = launchAff_ do
  let baseUrl = "https://api.github.com/repos/purescript/package-sets/releases"
  result <- Ajax.get ARF.json baseUrl
  releases <- recursivelyFetchReleases baseUrl [] 1
  let targetReleases = filter (\r -> not $ null r.assets) releases
  for_ targetReleases logShow
  pure unit

type ReleaseInfo =
  { name :: String
  , assets :: Array
    { name :: String
    , url :: String
    , id :: Int
    }
  }

releaseCodec :: JsonCodec ReleaseInfo
releaseCodec = CAR.object "Release"
  { name: CA.string
  , assets: CA.array $
    CAR.object "assets"
      { name: CA.string
      , url: CA.string
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
