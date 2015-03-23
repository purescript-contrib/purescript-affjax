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

traceAny' :: forall e. AffjaxResponse Unit -> Eff (trace :: Trace | e) Unit
traceAny' = traceAny

foreign import noContent "var noContent = new FormData();" :: RequestContent

-- TODO: make PR for options
instance isOptionUnit :: IsOption Unit where
  (:=) k a = (optionFn k) := toContent a

main = do

  go $ url := "/api"
     <> headers := [ContentType applicationOctetStream]
     <> content := noContent

  go $ url := "/api"
     <> method := POST
     <> content := unit

  launchAff $ do
    res <- attempt $ affjax $ url := "/api"
                           <> method := POST
                           <> content := unit
    liftEff $ case res of
      (Left err) -> traceAny err
      (Right res') -> traceAny (res' :: AffjaxResponse String)

go :: forall e a. (Requestable a) => Options (AffjaxOptions a) -> Eff (ajax :: Ajax, trace :: Trace | e) Unit
go opts = affjax' opts traceAny traceAny'
