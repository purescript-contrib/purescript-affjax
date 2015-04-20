module Network.HTTP.Affjax
  ( AJAX()
  , Affjax()
  , AffjaxRequest(), defaultRequest
  , AffjaxResponse()
  , URL()
  , affjax
  , affjax'
  , get
  , post, post_, post', post_'
  , put, put_, put', put_'
  , delete, delete_
  ) where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), error)
import Data.Either (Either(..))
import Data.Foreign (Foreign(..), F())
import Data.Function (Fn5(), runFn5, Fn4(), runFn4)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable(), toNullable)
import DOM.XHR (XMLHttpRequest())
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method (Method(..), methodToString)
import Network.HTTP.RequestHeader (RequestHeader(), requestHeaderName, requestHeaderValue)
import Network.HTTP.ResponseHeader (ResponseHeader(), responseHeader)
import Network.HTTP.StatusCode (StatusCode())

-- | The effect type for AJAX requests made with Affjax.
foreign import data AJAX :: !

-- | The type for Affjax requests.
type Affjax e a = Aff (ajax :: AJAX | e) (AffjaxResponse a)

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
affjax :: forall e a b. (Requestable a, Respondable b) => AffjaxRequest a -> Affjax e b
affjax = makeAff' <<< affjax'

-- | Makes a `GET` request to the specified URL.
get :: forall e a. (Respondable a) => URL -> Affjax e a
get u = affjax $ defaultRequest { url = u }

-- | Makes a `POST` request to the specified URL, sending data.
post :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax e b
post u c = affjax $ defaultRequest { method = POST, url = u, content = Just c }

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall e a b. (Requestable a, Respondable b) => URL -> Maybe a -> Affjax e b
post' u c = affjax $ defaultRequest { method = POST, url = u, content = c }

-- | Makes a `POST` request to the specified URL, sending data and ignoring the
-- | response.
post_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
post_ = post

-- | Makes a `POST` request to the specified URL with the option to send data,
-- | and ignores the response.
post_' :: forall e a. (Requestable a) => URL -> Maybe a -> Affjax e Unit
post_' = post'

-- | Makes a `PUT` request to the specified URL, sending data.
put :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax e b
put u c = affjax $ defaultRequest { method = PUT, url = u, content = Just c }

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall e a b. (Requestable a, Respondable b) => URL -> Maybe a -> Affjax e b
put' u c = affjax $ defaultRequest { method = PUT, url = u, content = c }

-- | Makes a `PUT` request to the specified URL, sending data and ignoring the
-- | response.
put_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
put_ = put

-- | Makes a `PUT` request to the specified URL with the option to send data,
-- | and ignores the response.
put_' :: forall e a. (Requestable a) => URL -> Maybe a -> Affjax e Unit
put_' = put'

-- | Makes a `DELETE` request to the specified URL.
delete :: forall e a. (Respondable a) => URL -> Affjax e a
delete u = affjax $ defaultRequest { method = DELETE, url = u }

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: forall e. URL -> Affjax e Unit
delete_ = delete

-- | Run a request directly without using `Aff`.
affjax' :: forall e a b. (Requestable a, Respondable b) =>
                         AffjaxRequest a ->
                         (Error -> Eff (ajax :: AJAX | e) Unit) ->
                         (AffjaxResponse b -> Eff (ajax :: AJAX | e) Unit) ->
                         Eff (ajax :: AJAX | e) (Canceler (ajax :: AJAX | e))
affjax' req eb cb =
  runFn5 _ajax responseHeader req' cancelAjax eb cb'
  where
  req' :: AjaxRequest
  req' = { method: methodToString req.method
         , url: req.url
         , headers: (\h -> { field: requestHeaderName h, value: requestHeaderValue h }) <$> req.headers
         , content: toNullable (toRequest <$> req.content)
         , responseType: responseTypeToString (responseType :: ResponseType b)
         , username: toNullable req.username
         , password: toNullable req.password
         }
  cb' :: AffjaxResponse ResponseContent -> Eff (ajax :: AJAX | e) Unit
  cb' res = case res { response = _  } <$> fromResponse res.response of
    Left err -> eb $ error (show err)
    Right res' -> cb res'

type AjaxRequest =
  { method :: String
  , url :: URL
  , headers :: [{ field :: String, value :: String }]
  , content :: Nullable RequestContent
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  }

foreign import _ajax
  """
  function _ajax (mkHeader, options, canceler, errback, callback) {
    return function () {
      var xhr = new XMLHttpRequest();
      xhr.open(options.method || "GET", options.url || "/", true, options.username, options.password);
      if (options.headers) {
        for (var i = 0, header; header = options.headers[i]; i++) {
          xhr.setRequestHeader(header.field, header.value);
        }
      }
      xhr.onerror = function (err) {
        errback(new Error("AJAX request failed: " + options.method + " " + options.url))();
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
      xhr.responseType = options.responseType;
      xhr.send(options.content);
      return canceler(xhr);
    };
  }
  """ :: forall e a. Fn5 (String -> String -> ResponseHeader)
                     AjaxRequest
                     (XMLHttpRequest -> Canceler (ajax :: AJAX | e))
                     (Error -> Eff (ajax :: AJAX | e) Unit)
                     (AffjaxResponse Foreign -> Eff (ajax :: AJAX | e) Unit)
                     (Eff (ajax :: AJAX | e) (Canceler (ajax :: AJAX | e)))

cancelAjax :: forall e. XMLHttpRequest -> Canceler (ajax :: AJAX | e)
cancelAjax xhr = Canceler \err -> makeAff (\eb cb -> runFn4 _cancelAjax xhr err eb cb)

foreign import _cancelAjax
  """
  function _cancelAjax (xhr, cancelError, errback, callback) {
    return function () {
      try { xhr.abort(); } catch (e) { return callback(false)(); }
      return callback(true)();
    };
  };
  """ :: forall e. Fn4 XMLHttpRequest
                       Error
                       (Error -> Eff (ajax :: AJAX | e) Unit)
                       (Boolean -> Eff (ajax :: AJAX | e) Unit)
                       (Eff (ajax :: AJAX | e) Unit)
