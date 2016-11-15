module Network.HTTP.Affjax
  ( AJAX
  , Affjax
  , AffjaxRequest, defaultRequest
  , AffjaxResponse
  , URL
  , affjax
  , affjax'
  , get
  , post, post_, post', post_'
  , put, put_, put', put_'
  , delete, delete_
  , patch, patch_, patch', patch_'
  , RetryDelayCurve
  , RetryPolicy(..)
  , defaultRetryPolicy
  , retry
  ) where

import Prelude hiding (max)

import Control.Monad.Aff (Aff, makeAff, makeAff', Canceler(..), attempt, later', forkAff, cancel)
import Control.Monad.Aff.AVar (AVAR, makeVar, takeVar, putVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept, throwError)

import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Foreign (Foreign, F, parseJSON, readString)
import Data.Function (on)
import Data.Function.Uncurried (Fn5, runFn5, Fn4, runFn4)
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.HTTP.Method as Method
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..), fst, snd)

import Math (max, pow)

import DOM.XHR.Types (XMLHttpRequest)

import Network.HTTP.Affjax.Request (class Requestable, RequestContent, toRequest)
import Network.HTTP.Affjax.Response (class Respondable, ResponseContent, ResponseType(..), fromResponse, responseType, responseTypeToString)
import Network.HTTP.RequestHeader (RequestHeader(..), requestHeaderName, requestHeaderValue)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeader)
import Network.HTTP.StatusCode (StatusCode(..))

-- | The effect type for AJAX requests made with Affjax.
foreign import data AJAX :: !

-- | The type for Affjax requests.
type Affjax e a = Aff (ajax :: AJAX | e) (AffjaxResponse a)

type AffjaxRequest a =
  { method :: Either Method CustomMethod
  , url :: URL
  , headers :: Array RequestHeader
  , content :: Maybe a
  , username :: Maybe String
  , password :: Maybe String
  , withCredentials :: Boolean
  }

defaultRequest :: AffjaxRequest Unit
defaultRequest =
  { method: Left GET
  , url: "/"
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: false
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
post u c = affjax $ defaultRequest { method = Left POST, url = u, content = Just c }

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall e a b. (Requestable a, Respondable b) => URL -> Maybe a -> Affjax e b
post' u c = affjax $ defaultRequest { method = Left POST, url = u, content = c }

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
put u c = affjax $ defaultRequest { method = Left PUT, url = u, content = Just c }

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall e a b. (Requestable a, Respondable b) => URL -> Maybe a -> Affjax e b
put' u c = affjax $ defaultRequest { method = Left PUT, url = u, content = c }

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
delete u = affjax $ defaultRequest { method = Left DELETE, url = u }

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: forall e. URL -> Affjax e Unit
delete_ = delete

-- | Makes a `PATCH` request to the specified URL, sending data.
patch :: forall e a b. (Requestable a, Respondable b) => URL -> a -> Affjax e b
patch u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = Just c }

-- | Makes a `PATCH` request to the specified URL with the option to send data.
patch' :: forall e a b. (Requestable a, Respondable b) => URL -> Maybe a -> Affjax e b
patch' u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = c }

-- | Makes a `PATCH` request to the specified URL, sending data and ignoring the
-- | response.
patch_ :: forall e a. (Requestable a) => URL -> a -> Affjax e Unit
patch_ = patch

-- | Makes a `PATCH` request to the specified URL with the option to send data,
-- | and ignores the response.
patch_' :: forall e a. (Requestable a) => URL -> Maybe a -> Affjax e Unit
patch_' = patch'

-- | A sequence of retry delays, in milliseconds.
type RetryDelayCurve = Int -> Int

-- | Expresses a policy for retrying Affjax requests with backoff.
type RetryPolicy =
  { timeout :: Maybe Int -- ^ the timeout in milliseconds, optional
  , delayCurve :: RetryDelayCurve
  , shouldRetryWithStatusCode :: StatusCode -> Boolean -- ^ whether a non-200 status code should trigger a retry
  }

-- | A sensible default for retries: no timeout, maximum delay of 30s, initial delay of 0.1s, exponential backoff, and no status code triggers a retry.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  { timeout : Nothing
  , delayCurve : \n -> round $ max (30.0 * 1000.0) $ 100.0 * (pow 2.0 $ toNumber (n - 1))
  , shouldRetryWithStatusCode : const false
  }

-- | Either we have a failure (which may be an exception or a failed response), or we have a successful response.
type RetryState e a = Either (Either e a) a

-- | Retry a request using a `RetryPolicy`. After the timeout, the last received response is returned; if it was not possible to communicate with the server due to an error, then this is bubbled up.
retry
  :: forall e a b
   . (Requestable a)
  => RetryPolicy
  -> (AffjaxRequest a -> Affjax (avar :: AVAR, ref :: REF | e) b)
  -> (AffjaxRequest a -> Affjax (avar :: AVAR, ref :: REF | e) b)
retry policy run req = do
  -- failureRef is either an exception or a failed request
  failureRef <- liftEff $ newRef Nothing
  let loop = go failureRef
  case policy.timeout of
    Nothing -> loop 1
    Just timeout -> do
      respVar <- makeVar
      loopHandle <- forkAff $ loop 1 >>= putVar respVar <<< Just
      timeoutHandle <-
        forkAff <<< later' timeout $ do
          putVar respVar Nothing
          loopHandle `cancel` error "Cancel"
      result <- takeVar respVar
      case result of
        Nothing -> do
          failure <- liftEff $ readRef failureRef
          case failure of
            Nothing -> throwError $ error "Timeout"
            Just failure' -> either throwError pure failure'
        Just resp -> pure resp
  where
    retryState
      :: Either Error (AffjaxResponse b)
      -> RetryState Error (AffjaxResponse b)
    retryState (Left exn) = Left $ Left exn
    retryState (Right resp) =
      case resp.status of
        StatusCode 200 -> Right resp
        code ->
          if policy.shouldRetryWithStatusCode code then
            Left $ Right resp
          else
            Right resp

    go failureRef n = do
      result <- retryState <$> attempt (run req)
      case result of
        Left err -> do
          liftEff $ writeRef failureRef $ Just err
          later' (policy.delayCurve n) $ go failureRef (n + 1)
        Right resp -> pure resp

-- | Run a request directly without using `Aff`.
affjax'
  :: forall e a b
   . (Requestable a, Respondable b)
  => AffjaxRequest a
  -> (Error -> Eff (ajax :: AJAX | e) Unit)
  -> (AffjaxResponse b -> Eff (ajax :: AJAX | e) Unit)
  -> Eff (ajax :: AJAX | e) (Canceler (ajax :: AJAX | e))
affjax' req eb cb =
  runFn5 _ajax responseHeader req' cancelAjax eb cb'
  where

  req' :: AjaxRequest
  req' =
    { method: Method.print req.method
    , url: req.url
    , headers: (\h -> { field: requestHeaderName h, value: requestHeaderValue h }) <$> headers
    , content: toNullable (snd requestSettings)
    , responseType: responseTypeToString (snd responseSettings)
    , username: toNullable req.username
    , password: toNullable req.password
    , withCredentials: req.withCredentials
    }

  requestSettings :: Tuple (Maybe MediaType) (Maybe RequestContent)
  requestSettings = case toRequest <$> req.content of
    Nothing -> Tuple Nothing Nothing
    Just (Tuple mime rt) -> Tuple mime (Just rt)

  responseSettings :: Tuple (Maybe MediaType) (ResponseType b)
  responseSettings = responseType

  headers :: Array RequestHeader
  headers =
    addHeader (ContentType <$> fst requestSettings) $
      addHeader (Accept <$> fst responseSettings)
        req.headers

  addHeader :: Maybe RequestHeader -> Array RequestHeader -> Array RequestHeader
  addHeader mh hs = case mh of
    Just h | not $ any (on eq requestHeaderName h) hs -> hs `Arr.snoc` h
    _ -> hs

  cb' :: AffjaxResponse ResponseContent -> Eff (ajax :: AJAX | e) Unit
  cb' res = case res { response = _  } <$> runExcept (fromResponse' res.response) of
    Left err -> eb $ error (show err)
    Right res' -> cb res'

  fromResponse' :: ResponseContent -> F b
  fromResponse' = case snd responseSettings of
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
  , withCredentials :: Boolean
  }

foreign import _ajax
  :: forall e. Fn5 (String -> String -> ResponseHeader)
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
