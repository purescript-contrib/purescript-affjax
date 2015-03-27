module Network.HTTP.Affjax
  ( Ajax()
  , Affjax()
  , AffjaxOptions()
  , AffjaxResponse()
  , URL()
  , url, method, content, headers, username, password
  , affjax
  , affjax'
  , get
  , post, post_
  , put, put_
  , delete, delete_
  ) where

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), error)
import Data.Either (Either(..))
import Data.Foreign (Foreign(..), F())
import Data.Function (Fn4(), runFn4)
import Data.Options (Option(), Options(), options, (:=), opt)
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.ResponseType
import Network.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader())
import Network.HTTP.ResponseHeader (ResponseHeader(), responseHeader)
import Network.HTTP.StatusCode (StatusCode())

-- | The effect type for AJAX requests made with Affjax.
foreign import data Ajax :: !

-- | The type for Affjax requests.
type Affjax e a = Aff (ajax :: Ajax | e) (AffjaxResponse a)

-- | Options type for Affjax requests.
foreign import data AffjaxOptions :: *

-- | The type of records that will be received as an Affjax response.
type AffjaxResponse a =
  { status :: StatusCode
  , headers :: [ResponseHeader]
  , response :: a
  }

-- | Type alias for URL strings to aid readability of types.
type URL = String

-- | Sets the URL for a request.
url :: Option AffjaxOptions URL
url = opt "url"

-- | Sets the HTTP method for a request.
method :: Option AffjaxOptions Method
method = opt "method"

-- | Sets the content to send in a request.
content :: Option AffjaxOptions RequestContent
content = opt "content"

-- | Sets the headers to send with a request.
headers :: Option AffjaxOptions [RequestHeader]
headers = opt "headers"

-- | Sets the HTTP auth username to send with a request.
username :: Option AffjaxOptions String
username = opt "username"

-- | Sets the HTTP auth password to send with a request.
password :: Option AffjaxOptions String
password = opt "password"

-- | Sets the expected response type for a request. This is not exposed outside
-- | of the module as the `ResponseType` is set based on the `Responsable`
-- | instance for the expected result content type.
responseType = opt "responseType" :: Option AffjaxOptions ResponseType

-- | Runs a request.
affjax :: forall e a. Responsable a ->
                      Options AffjaxOptions ->
                      Affjax e a
affjax r = makeAff <<< affjax' r

-- | Runs a request directly in Eff.
affjax' :: forall e a. Responsable a ->
                       Options AffjaxOptions ->
                       (Error -> Eff (ajax :: Ajax | e) Unit) ->
                       (AffjaxResponse a -> Eff (ajax :: Ajax | e) Unit) ->
                       Eff (ajax :: Ajax | e) Unit
affjax' (Responsable read ty) opts eb cb =
  runFn4 unsafeAjax responseHeader (options $ opts <> responseType := ty) eb cb'
  where
  cb' :: AffjaxResponse Foreign -> Eff (ajax :: Ajax | e) Unit
  cb' res = case res { response = _  } <$> read res.response of
    Left err -> eb $ error (show err)
    Right res' -> cb res'

get :: forall e a. URL -> Responsable a -> Affjax e a
get u r = affjax r $ method := GET
                  <> url := u

post :: forall e a. URL -> Responsable a -> RequestContent -> Affjax e a
post u r c = affjax r $ method := POST
                     <> url := u
                     <> content := c

post_ :: forall e. URL -> RequestContent -> Affjax e Unit
post_ = flip post rUnit

put :: forall e a. URL -> Responsable a -> RequestContent -> Affjax e a
put u r c = affjax r $ method := PUT
                    <> url := u
                    <> content := c

put_ :: forall e. URL -> RequestContent -> Affjax e Unit
put_ = flip put rUnit

delete :: forall e a. URL -> Responsable a -> Affjax e a
delete u r = affjax r $ method := DELETE
                     <> url := u

delete_ :: forall e. URL -> Affjax e Unit
delete_ = flip delete rUnit

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
  """ :: forall e a. Fn4 (String -> String -> ResponseHeader)
                     Foreign
                     (Error -> Eff (ajax :: Ajax | e) Unit)
                     (AffjaxResponse Foreign -> Eff (ajax :: Ajax | e) Unit)
                     (Eff (ajax :: Ajax | e) Unit)
