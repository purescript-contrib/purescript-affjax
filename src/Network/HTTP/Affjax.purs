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
  , retry
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Bind ((<=<))
import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..), attempt, later', forkAff, cancel)
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), error)
import Data.Either (Either(..))
import Data.Foreign (Foreign(..), F(), parseJSON, readString)
import Data.Function (Fn5(), runFn5, Fn4(), runFn4)
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable(), toNullable)
import DOM.XHR (XMLHttpRequest())
import Math (pow)
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method (Method(..), methodToString)
import Network.HTTP.RequestHeader (RequestHeader(), requestHeaderName, requestHeaderValue)
import Network.HTTP.ResponseHeader (ResponseHeader(), responseHeader)
import Network.HTTP.StatusCode (StatusCode(..))

-- | The effect type for AJAX requests made with Affjax.
foreign import data AJAX :: !

-- | The type for Affjax requests.
type Affjax e a = Aff (ajax :: AJAX | e) (AffjaxResponse a)

type AffjaxRequest a =
  { method :: Method
  , url :: URL
  , headers :: Array RequestHeader
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
  , headers :: Array ResponseHeader
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

-- | Retry a request with exponential backoff, timing out optionally after a specified number of milliseconds.
retry :: forall e a b. (Requestable a) => Maybe Int -> (AffjaxRequest a -> Affjax (avar :: AVAR | e) b) -> (AffjaxRequest a -> Affjax (avar :: AVAR | e) b)
retry milliseconds run req = do
  failureVar <- makeVar
  let loop = go failureVar
  case milliseconds of
    Nothing -> loop 1
    Just milliseconds -> do
      respVar <- makeVar
      loopHandle <- forkAff $ loop 1 >>= putVar respVar <<< Just
      timeoutHandle <-
        forkAff <<< later' milliseconds $ do
          putVar respVar Nothing
          loopHandle `cancel` error "Cancel"
      result <- takeVar respVar
      case result of
        Nothing ->
          takeVar failureVar
        Just resp -> pure resp
  where
    assert200 resp =
      case resp.status of
        StatusCode 200 -> Right resp
        _ -> Left resp

    go failureVar n = do
      result <- run req
      case assert200 result of
        Right b -> pure b
        Left resp -> do
          putVar failureVar resp
          -- TODO: is this too steep?
          let delay = round $ toNumber 1000 * pow (toNumber 2) (toNumber (n - 1))
          later' delay $ go failureVar (n + 1)

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
  cb' res = case res { response = _  } <$> fromResponse' res.response of
    Left err -> eb $ error (show err)
    Right res' -> cb res'
  fromResponse' :: ResponseContent -> F b
  fromResponse' = case (responseType :: ResponseType b) of
    JSONResponse -> fromResponse <=< parseJSON <=< readString
    _ -> fromResponse

type AjaxRequest =
  { method :: String
  , url :: URL
  , headers :: Array { field :: String, value :: String }
  , content :: Nullable RequestContent
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  }

foreign import _ajax
  :: forall e a. Fn5 (String -> String -> ResponseHeader)
                 AjaxRequest
                 (XMLHttpRequest -> Canceler (ajax :: AJAX | e))
                 (Error -> Eff (ajax :: AJAX | e) Unit)
                 (AffjaxResponse Foreign -> Eff (ajax :: AJAX | e) Unit)
                 (Eff (ajax :: AJAX | e) (Canceler (ajax :: AJAX | e)))

cancelAjax :: forall e. XMLHttpRequest -> Canceler (ajax :: AJAX | e)
cancelAjax xhr = Canceler \err -> makeAff (\eb cb -> runFn4 _cancelAjax xhr err eb cb)

foreign import _cancelAjax
  :: forall e. Fn4 XMLHttpRequest
                   Error
                   (Error -> Eff (ajax :: AJAX | e) Unit)
                   (Boolean -> Eff (ajax :: AJAX | e) Unit)
                   (Eff (ajax :: AJAX | e) Unit)

