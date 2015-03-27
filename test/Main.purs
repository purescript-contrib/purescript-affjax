module Test.Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Either
import Data.Foreign
import Data.Options
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

foreign import noContent "var noContent = new FormData();" :: RequestContent

main = do

  go $ url := "/api"
     <> headers := [ContentType applicationOctetStream]
     <> content := (toContent "test")

  go $ url := "/api"
     <> method := POST
     <> content := (toContent unit)

  launchAff $ do
    res <- attempt $ affjax rString $ url := "/api" <> method := POST
    liftEff $ either traceAny traceAny res

  launchAff $ do
    res <- attempt $ get "/arrayview" rInt8Array
    liftEff $ either traceAny traceAny res

go :: forall e. Options AffjaxOptions -> Eff (ajax :: Ajax, trace :: Trace | e) Unit
go opts = affjax' rUnit opts traceAny traceAny
