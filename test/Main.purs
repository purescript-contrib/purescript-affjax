module Test.Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Either
import Data.Foreign
import Debug.Trace
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

foreign import traceAny
  """
  function traceAny(a){
    return function () {
      console.log(a);
      return {};
    };
  }
  """ :: forall e a. a -> Eff (trace :: Trace | e) Unit

main = launchAff $ do

  res <- attempt $ affjax $ defaultRequest { url = "/api", method = POST }
  liftEff $ either traceAny (traceAny :: AffjaxResponse String -> _) res

  res <- attempt $ post_ "/api" "test"
  liftEff $ either traceAny traceAny res

  res <- attempt $ get "/arrayview"
  liftEff $ either traceAny (traceAny :: AffjaxResponse Foreign -> _) res

  res <- attempt $ get "ttp://www.google.com"
  liftEff $ either traceAny (traceAny :: AffjaxResponse Foreign -> _) res

  canceler <- forkAff (post_ "/api" "do it now")
  canceled <- canceler `cancel` error "Pull the cord!"
  liftEff $ if canceled then (trace "Canceled") else (trace "Not Canceled")
