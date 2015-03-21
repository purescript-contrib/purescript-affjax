module Network.Affjax.Request
  ( Ajax()
  , AjaxContent()
  , AjaxRequest()
  , AjaxResponse()
  , defaultRequest
  , ajax
  ) where

import Control.Monad.Aff (Aff(), EffA(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())
import Data.Function (Fn8(), runFn8)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable(), toNullable)
import Network.HTTP.Method (Method(..), methodToString)
import Network.HTTP.RequestHeader (RequestHeader(), requestHeaderName, requestHeaderValue)

-- | The event type for AJAX requests.
foreign import data Ajax :: !

-- | Type subsuming all content types that can be sent in a request.
foreign import data AjaxContent :: *

-- | The parameters for an AJAX request.
type AjaxRequest =
  { url :: String
  , method :: Method
  , headers :: [RequestHeader]
  , content :: Maybe AjaxContent
  , username :: Maybe String
  , password :: Maybe String
  }

-- TODO: probably not this? Do we want to deal with other responses, include headers, etc?
newtype AjaxResponse = AjaxResponse String

-- | A basic request, `GET /` with no particular headers or credentials.
defaultRequest :: AjaxRequest
defaultRequest =
  { url: "/"
  , method: GET
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  }

-- | Make an AJAX request.
ajax :: forall e. AjaxRequest -> Aff (ajax :: Ajax | e) AjaxResponse
ajax req = makeAff $ runFn8
  unsafeAjax req.url
             (methodToString req.method)
             (runHeader <$> req.headers)
             (toNullable req.content)
             (toNullable req.username)
             (toNullable req.password)
  where
  runHeader :: RequestHeader -> { head :: String, value :: String }
  runHeader h = { head: requestHeaderName h, value: requestHeaderValue h }

foreign import unsafeAjax
  """
  function unsafeAjax (url, method, headers, content, username, password, errback, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, true, username, password);
    for (var i = 0, header; header = headers[i]; i++) {
      xhr.setRequestHeader(header.head, header.value);
    }
    xhr.onerror = function (err) {
      errback(err)();
    };
    xhr.onload = function () {
      if (xhr.status === 200) callback(xhr.response)();
      else errback(new Error("Request returned status " + xhr.status))();
    };
    xhr.send(content);
  }
  """ :: forall e. Fn8 String
                       String
                       [{ head :: String, value :: String }]
                       (Nullable AjaxContent)
                       (Nullable String)
                       (Nullable String)
                       (Error -> Eff (ajax :: Ajax | e) Unit)
                       (AjaxResponse -> Eff (ajax :: Ajax | e) Unit)
                       (EffA (ajax :: Ajax | e) Unit)
