module Network.HTTP.Affjax
  ( Ajax()
  , Affjax()
  , AffjaxRequest(), defaultRequest
  , AffjaxResponse()
  , URL()
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
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign (Foreign(..), F())
import Data.Function (Fn4(), runFn4)
import Data.Nullable (Nullable(), toNullable)
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.ResponseType
import Network.HTTP.Method (Method(..), methodToString)
import Network.HTTP.RequestHeader (RequestHeader(), requestHeaderName, requestHeaderValue)
import Network.HTTP.ResponseHeader (ResponseHeader(), responseHeader)
import Network.HTTP.StatusCode (StatusCode())

-- | The effect type for AJAX requests made with Affjax.
foreign import data Ajax :: !

-- | The type for Affjax requests.
type Affjax e a = Aff (ajax :: Ajax | e) (AffjaxResponse a)

type AffjaxRequest a =
  { method :: Method
  , url :: URL
  , headers :: [RequestHeader]
  , content :: Maybe a
  , username :: Maybe String
  , password :: Maybe String
  }

defaultRequest :: AffjaxRequest Unit
defaultRequest =
  { method: GET
  , url: "/"
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  }

-- | The type of records that will be received as an Affjax response.
type AffjaxResponse a =
  { status :: StatusCode
  , headers :: [ResponseHeader]
  , response :: a
  }

-- | Type alias for URL strings to aid readability of types.
type URL = String

-- | Makes an `Affjax` request.
affjax :: forall e a b. (Requestable a, Responsable b) => AffjaxRequest a -> Affjax e b
affjax = makeAff <<< affjax'

get :: forall e a. (Responsable a) => URL -> Affjax e a
get u = affjax $ defaultRequest { url = u }

post :: forall e a b. (Requestable a, Responsable b) => URL -> a -> Affjax e b
post u c = affjax $ defaultRequest { method = POST, url = u, content = Just c }

post_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
post_ = post

put :: forall e a b. (Requestable a, Responsable b) => URL -> a -> Affjax e b
put u c = affjax $ defaultRequest { method = PUT, url = u, content = Just c }

put_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
put_ = put

delete :: forall e a. (Responsable a) => URL -> Affjax e a
delete u = affjax $ defaultRequest { method = DELETE, url = u }

delete_ :: forall e. URL -> Affjax e Unit
delete_ = delete

-- | Run a request directly without using `Aff`.
affjax' :: forall e a b. (Requestable a, Responsable b) =>
                         AffjaxRequest a ->
                         (Error -> Eff (ajax :: Ajax | e) Unit) ->
                         (AffjaxResponse b -> Eff (ajax :: Ajax | e) Unit) ->
                         Eff (ajax :: Ajax | e) Unit
affjax' req eb cb =
  runFn4 unsafeAjax responseHeader req' eb cb'
  where
  req' :: AjaxRequest
  req' = { method: methodToString req.method
         , url: req.url
         , headers: (\h -> { field: requestHeaderName h, value: requestHeaderValue h }) <$> req.headers
         , content: toNullable (toRequest <$> req.content)
         , username: toNullable req.username
         , password: toNullable req.password
         }
  cb' :: AffjaxResponse ResponseContent -> Eff (ajax :: Ajax | e) Unit
  cb' res = case res { response = _  } <$> fromResponse res.response of
    Left err -> eb $ error (show err)
    Right res' -> cb res'

type AjaxRequest =
  { method :: String
  , url :: URL
  , headers :: [{ field :: String, value :: String }]
  , content :: Nullable RequestContent
  , username :: Nullable String
  , password :: Nullable String
  }

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
                     AjaxRequest
                     (Error -> Eff (ajax :: Ajax | e) Unit)
                     (AffjaxResponse Foreign -> Eff (ajax :: Ajax | e) Unit)
                     (Eff (ajax :: Ajax | e) Unit)
