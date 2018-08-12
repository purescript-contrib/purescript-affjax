module Test.DocExamples where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (log)

main :: Effect Unit
main = void $ launchAff $ do
  res <- AX.request ResponseFormat.json (AX.defaultRequest { url = "/api", method = Left GET })
  case res.body of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "GET /api response: " <> J.stringify json

main' :: Effect Unit
main' = void $ launchAff $ do
  res1 <- AX.get ResponseFormat.json "/api"
  case res1.body of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "GET /api response: " <> J.stringify json

  res2 <- AX.post ResponseFormat.json "/api" (RequestBody.json (J.fromString "test"))
  case res2.body of
    Left err -> log $ "POST /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "POST /api response: " <> J.stringify json
