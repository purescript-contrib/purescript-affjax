module Network.Affjax.Request
  ( Ajax()
  , AjaxRequest()
  , Content(..)
  , AjaxResponse()
  , defaultRequest
  , ajax
  ) where

import Control.Monad.Aff (Aff(), EffA(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception(Error())
import Data.Array ()
import Data.ArrayBuffer.Types (ArrayView())
import Data.Function (Fn8(), runFn8)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable(), toNullable)
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.HTTP.Method
import Network.HTTP.RequestHeader

-- | The event type for AJAX requests.
foreign import data Ajax :: !

-- | The parameters for an AJAX request.
type AjaxRequest a =
  { url :: String
  , method :: Method
  , headers :: [RequestHeader]
  , content :: Maybe (Content a)
  , username :: Maybe String
  , password :: Maybe String
  }

-- | The types of data that can be set in an AJAX request.
data Content a
  = ArrayViewContent (ArrayView a)
  | BlobContent Blob
  | DocumentContent Document
  | TextContent String
  | FormDataContent FormData

-- TODO: probably not this? Do we want to deal with other responses, include headers, etc?
newtype AjaxResponse = AjaxResponse String

-- | A basic request, `GET /` with no particular headers or credentials.
defaultRequest :: forall c. AjaxRequest c
defaultRequest =
  { url: "/"
  , method: GET
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  }

-- | Make an AJAX request.
ajax :: forall e a. AjaxRequest a -> Aff (ajax :: Ajax | e) AjaxResponse
ajax req = makeAff $ runFn8
  unsafeAjax req.url
             (methodToString req.method)
             (runHeader <$> req.headers)
             (toNullable $ runContent <$> req.content)
             (toNullable req.username)
             (toNullable req.password)
  where
  runHeader :: RequestHeader -> { head :: String, value :: String }
  runHeader h = { head: requestHeaderName h, value: requestHeaderValue h }
  runContent :: forall c. Content c -> XHRContent
  runContent (ArrayViewContent av) = unsafeToXHRContent av
  runContent (BlobContent b) = unsafeToXHRContent b
  runContent (DocumentContent d) = unsafeToXHRContent d
  runContent (TextContent s) = unsafeToXHRContent s
  runContent (FormDataContent fd) = unsafeToXHRContent fd

foreign import data XHRContent :: *

foreign import unsafeToXHRContent
  """
  function unsafeToXHRContent (value) {
    return value;
  }
  """ :: forall a. a -> XHRContent

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
                       (Nullable XHRContent)
                       (Nullable String)
                       (Nullable String)
                       (Error -> Eff (ajax :: Ajax | e) Unit)
                       (AjaxResponse -> Eff (ajax :: Ajax | e) Unit)
                       (EffA (ajax :: Ajax | e) Unit)
