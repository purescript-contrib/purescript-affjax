module Test.DocExamples where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

main = launchAff $ do
  res <- AX.request ResponseFormat.json (AX.defaultRequest { url = "/api", method = Left GET })
  liftEffect $ log $ "GET /api response: " <> J.stringify res.response

main' = launchAff $ do
  res1 <- AX.get ResponseFormat.json "/api"
  liftEffect $ log $ "GET /api response: " <> J.stringify res1.response

  res2 <- AX.post ResponseFormat.json "/api" (RequestBody.json (J.fromString "test"))
  liftEffect $ log $ "POST /api response: " <> J.stringify res2.response
