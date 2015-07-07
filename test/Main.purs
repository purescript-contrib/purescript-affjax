module Test.Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception
import Data.Either
import Data.Foreign
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

foreign import logAny
  :: forall e a. a -> Eff (console :: CONSOLE | e) Unit

main = launchAff $ do

  res <- attempt $ affjax $ defaultRequest { url = "/api", method = POST }
  liftEff $ either logAny (logAny :: AffjaxResponse String -> _) res

  res <- attempt $ post_ "/api" "test"
  liftEff $ either logAny logAny res

  res <- attempt $ get "/arrayview"
  liftEff $ either logAny (logAny :: AffjaxResponse Foreign -> _) res

  res <- attempt $ get "ttp://www.google.com"
  liftEff $ either logAny (logAny :: AffjaxResponse Foreign -> _) res

  canceler <- forkAff (post_ "/api" "do it now")
  canceled <- canceler `cancel` error "Pull the cord!"
  liftEff $ if canceled then (log "Canceled") else (log "Not Canceled")
