module Network.HTTP.Affjax
  ( Ajax()
  , AffjaxOptions()
  , AffjaxResponse()
  , url, method, content, headers, username, password
  , affjax
  , affjax'
  ) where

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())
import Data.Foreign (Foreign(..))
import Data.Function (Fn4(), runFn4)
import Data.Options (Option(), Options(), IsOption, options, (:=), opt)
import Data.Proxy (Proxy(..))
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.ResponseType
import Network.HTTP.Method (Method())
import Network.HTTP.RequestHeader (RequestHeader())
import Network.HTTP.ResponseHeader (ResponseHeader(), responseHeader)
import Network.HTTP.StatusCode (StatusCode())

-- | The effect type for AJAX requests made with Affjax.
foreign import data Ajax :: !

-- | Options type for Affjax requests.
foreign import data AffjaxOptions :: * -> *

-- | The type of records that will be received as an Affjax response.
type AffjaxResponse a =
  { status :: StatusCode
  , headers :: [ResponseHeader]
  , response :: a
  }

-- | Sets the URL for a request.
url :: forall a. Option (AffjaxOptions a) String
url = opt "url"

-- | Sets the HTTP method for a request.
method :: forall a. Option (AffjaxOptions a) Method
method = opt "method"

-- | Sets the content to send in a request.
content :: forall a. (Requestable a, IsOption a) => Option (AffjaxOptions a) a
content = opt "content"

-- | Sets the headers to send with a request.
headers :: forall a. Option (AffjaxOptions a) [RequestHeader]
headers = opt "headers"

-- | Sets the HTTP auth username to send with a request.
username :: forall a. Option (AffjaxOptions a) String
username = opt "username"

-- | Sets the HTTP auth password to send with a request.
password :: forall a. Option (AffjaxOptions a) String
password = opt "password"

-- | Sets the expected response type for a request. This is not exposed outside
-- | of the module as the `ResponseType` is set based on the `Responsable`
-- | instance for the expected result content type.
responseType = opt "responseType" :: forall a. Option (AffjaxOptions a) ResponseType

-- | Runs a request.
affjax :: forall e a b. (Requestable a, Responsable b) =>
                        Options (AffjaxOptions a) ->
                        Aff (ajax :: Ajax | e) (AffjaxResponse b)
affjax = makeAff <<< affjax'

-- | Runs a request directly in Eff.
affjax' :: forall e a b. (Requestable a, Responsable b) =>
                         Options (AffjaxOptions a) ->
                         (Error -> Eff (ajax :: Ajax | e) Unit) ->
                         (AffjaxResponse b -> Eff (ajax :: Ajax | e) Unit) ->
                         Eff (ajax :: Ajax | e) Unit
affjax' opts eb cb =
  let opts' = opts <> responseType := toResponseType (Proxy :: Proxy b)
  in runFn4 unsafeAjax responseHeader (options opts') eb cb

foreign import unsafeAjax
  """
  function unsafeAjax (mkHeader, options, errback, callback) {
    return function () {
      var xhr = new XMLHttpRequest();
      xhr.open(options.method || "GET", options.url || "/", true, options.username, options.password);
      if (options.headers) {
        for (var i = 0, header; header = options.headers[i]; i++) {
          xhr.setRequestHeader(header.field, header.value);
        }
      }
      xhr.onerror = function (err) {
        errback(err)();
      };
      xhr.onload = function () {
        callback({
          status: xhr.status,
          headers: xhr.getAllResponseHeaders().split("\n")
            .filter(function (header) {
              return header.length > 0;
            })
            .map(function (header) {
              var i = header.indexOf(":");
              return mkHeader(header.substring(0, i))(header.substring(i + 2));
            }),
          response: xhr.response
        })();
      };
      if (options.responseType) xhr.responseType = options.responseType;
      xhr.send(options.content);
    };
  }
  """ :: forall e a b. Fn4 (String -> String -> ResponseHeader)
                       Foreign
                       (Error -> Eff (ajax :: Ajax | e) Unit)
                       (AffjaxResponse b -> Eff (ajax :: Ajax | e) Unit)
                       (Eff (ajax :: Ajax | e) Unit)
