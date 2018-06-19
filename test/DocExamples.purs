module Test.DocExamples where

import Prelude

import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request as AXReq
import Network.HTTP.Affjax.Response as AXRes

main = launchAff $ do
  res <- AX.affjax AXRes.json (AX.defaultRequest { url = "/api", method = Left GET })
  liftEffect $ log $ "GET /api response: " <> J.stringify res.response

main' = launchAff $ do
  res1 <- AX.get AXRes.json "/api"
  liftEffect $ log $ "GET /api response: " <> J.stringify res1.response

  res2 <- AX.post AXRes.json "/api" (AXReq.json (J.fromString "test"))
  liftEffect $ log $ "POST /api response: " <> J.stringify res2.response
