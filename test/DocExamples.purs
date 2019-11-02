module Test.DocExamples where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (log)

main :: Effect Unit
main = void $ launchAff $ do
  result <- AX.request (AX.defaultRequest { url = "/api", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> J.stringify response.body

main' :: Effect Unit
main' = void $ launchAff $ do
  result1 <- AX.get ResponseFormat.json "/api"
  case result1 of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> log $ "GET /api response: " <> J.stringify response.body

  result2 <- AX.post ResponseFormat.json "/api" (Just (RequestBody.json (J.fromString "test")))
  case result2 of
    Left err -> log $ "POST /api response failed to decode: " <> AX.printError err
    Right response -> log $ "POST /api response: " <> J.stringify response.body
