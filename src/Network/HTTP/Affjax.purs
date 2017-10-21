module Network.HTTP.Affjax
  ( AJAX
  , Affjax
  , AffjaxRequest, defaultRequest
  , AffjaxResponse
  , URL
  , affjax
  , get
  , post, post_, post', post_'
  , put, put_, put', put_'
  , delete, delete_
  , patch, patch_, patch', patch_'
  , RetryDelayCurve
  , RetryPolicy(..)
  , defaultRetryPolicy
  , retry
  , module Exports
  ) where

import Prelude

import Control.Monad.Aff (Aff, try, delay)
import Control.Monad.Aff.Compat as AC
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept, throwError)
import Control.Parallel (parOneOf)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Foreign (F, Foreign, ForeignError(JSONError), fail, readString, toForeign)
import Data.FormURLEncoded as FormURLEncoded
import Data.Function (on)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.HTTP.Method as Method
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple, fst, snd)
import Math as Math
import Network.HTTP.Affjax.Request (RequestContent(..)) as Exports
import Network.HTTP.Affjax.Request (RequestContent(..), defaultMediaType)
import Network.HTTP.Affjax.Response (class Respondable, ResponseContent, ResponseType(..), fromResponse, responseType, responseTypeToString)
import Network.HTTP.RequestHeader (RequestHeader(..), requestHeaderName, requestHeaderValue)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeader)
import Network.HTTP.StatusCode (StatusCode(..))

-- | The effect type for AJAX requests made with Affjax.
foreign import data AJAX :: Effect

-- | The result type for Affjax requests.
type Affjax eff a = Aff (ajax :: AJAX | eff) (AffjaxResponse a)

type AffjaxRequest =
  { method :: Either Method CustomMethod
  , url :: URL
  , headers :: Array RequestHeader
  , content :: Maybe RequestContent
  , username :: Maybe String
  , password :: Maybe String
  , withCredentials :: Boolean
  }

defaultRequest :: AffjaxRequest
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

-- | Makes a `GET` request to the specified URL.
get :: forall e a. Respondable a => URL -> Affjax e a
get u = affjax $ defaultRequest { url = u }

-- | Makes a `POST` request to the specified URL, sending data.
post :: forall e a b. Respondable b => URL -> RequestContent -> Affjax e b
post u c = affjax $ defaultRequest { method = Left POST, url = u, content = Just c }

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall e a b. Respondable b => URL -> Maybe RequestContent -> Affjax e b
post' u c = affjax $ defaultRequest { method = Left POST, url = u, content = c }

-- | Makes a `POST` request to the specified URL, sending data and ignoring the
-- | response.
post_ :: forall e a. URL -> RequestContent -> Affjax e Unit
post_ = post

-- | Makes a `POST` request to the specified URL with the option to send data,
-- | and ignores the response.
post_' :: forall e a. URL -> Maybe RequestContent -> Affjax e Unit
post_' = post'

-- | Makes a `PUT` request to the specified URL, sending data.
put :: forall e a b. Respondable b => URL -> RequestContent -> Affjax e b
put u c = affjax $ defaultRequest { method = Left PUT, url = u, content = Just c }

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall e a b. Respondable b => URL -> Maybe RequestContent -> Affjax e b
put' u c = affjax $ defaultRequest { method = Left PUT, url = u, content = c }

-- | Makes a `PUT` request to the specified URL, sending data and ignoring the
-- | response.
put_ :: forall e a. URL -> RequestContent -> Affjax e Unit
put_ = put

-- | Makes a `PUT` request to the specified URL with the option to send data,
-- | and ignores the response.
put_' :: forall e a. URL -> Maybe RequestContent -> Affjax e Unit
put_' = put'

-- | Makes a `DELETE` request to the specified URL.
delete :: forall e a. Respondable a => URL -> Affjax e a
delete u = affjax $ defaultRequest { method = Left DELETE, url = u }

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: forall e. URL -> Affjax e Unit
delete_ = delete

-- | Makes a `PATCH` request to the specified URL, sending data.
patch :: forall e a b. Respondable b => URL -> RequestContent -> Affjax e b
patch u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = Just c }

-- | Makes a `PATCH` request to the specified URL with the option to send data.
patch' :: forall e a b. Respondable b => URL -> Maybe RequestContent -> Affjax e b
patch' u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = c }

-- | Makes a `PATCH` request to the specified URL, sending data and ignoring the
-- | response.
patch_ :: forall e a. URL -> RequestContent -> Affjax e Unit
patch_ = patch

-- | Makes a `PATCH` request to the specified URL with the option to send data,
-- | and ignores the response.
patch_' :: forall e a. URL -> Maybe RequestContent -> Affjax e Unit
patch_' = patch'

-- | A sequence of retry delays, in milliseconds.
type RetryDelayCurve = Int -> Milliseconds

-- | Expresses a policy for retrying Affjax requests with backoff.
type RetryPolicy =
  { timeout :: Maybe Milliseconds -- ^ the timeout in milliseconds, optional
  , delayCurve :: RetryDelayCurve
  , shouldRetryWithStatusCode :: StatusCode -> Boolean -- ^ whether a non-200 status code should trigger a retry
  }

-- | A sensible default for retries: no timeout, maximum delay of 30s, initial delay of 0.1s, exponential backoff, and no status code triggers a retry.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  { timeout : Nothing
  , delayCurve : \n -> Milliseconds $ max (30.0 * 1000.0) $ 100.0 * (Math.pow 2.0 $ toNumber (n - 1))
  , shouldRetryWithStatusCode : const false
  }

-- | Either we have a failure (which may be an exception or a failed response), or we have a successful response.
type RetryState e a = Either (Either e a) a

-- | Retry a request using a `RetryPolicy`. After the timeout, the last received response is returned; if it was not possible to communicate with the server due to an error, then this is bubbled up.
retry
  :: forall e a b
   . RetryPolicy
  -> (AffjaxRequest -> Affjax (ref :: REF | e) b)
  -> AffjaxRequest
  -> Affjax (ref :: REF | e) b
retry policy run req = do
  -- failureRef is either an exception or a failed request
  failureRef <- liftEff $ newRef Nothing
  let loop = go failureRef
  case policy.timeout of
    Nothing -> loop 1
    Just timeout -> do
      result <- parOneOf [ Just <$> loop 1, Nothing <$ delay timeout ]
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
      result <- retryState <$> try (run req)
      case result of
        Left err -> do
          liftEff $ writeRef failureRef $ Just err
          delay (policy.delayCurve n)
          go failureRef (n + 1)
        Right resp -> pure resp

-- | Makes an `Affjax` request.
affjax
  :: forall e a b
   . Respondable b
  => AffjaxRequest
  -> Affjax e b
affjax req = do
  res <- AC.fromEffFnAff $ runFn2 _ajax responseHeader req'
  case res { response = _  } <$> runExcept (fromResponse' res.response) of
    Left err -> throwError $ error (show err)
    Right res' -> pure res'
  where

  req' :: AjaxRequest
  req' =
    { method: Method.print req.method
    , url: req.url
    , headers: (\h -> { field: requestHeaderName h, value: requestHeaderValue h }) <$> headers req.content
    , content: toNullable (extractContent <$> req.content)
    , responseType: responseTypeToString (snd responseSettings)
    , username: toNullable req.username
    , password: toNullable req.password
    , withCredentials: req.withCredentials
    }

  extractContent :: RequestContent -> Foreign
  extractContent = case _ of
    ArrayViewInt8Request x → toForeign x
    ArrayViewInt16Request x → toForeign x
    ArrayViewInt32Request x → toForeign x
    ArrayViewUint8Request x → toForeign x
    ArrayViewUint16Request x → toForeign x
    ArrayViewUint32Request x → toForeign x
    ArrayViewUint8ClampedRequest x → toForeign x
    ArrayViewFloat32Request x → toForeign x
    ArrayViewFloat64Request x → toForeign x
    BlobRequest x → toForeign x
    DocumentRequest x → toForeign x
    StringRequest x → toForeign x
    FormDataRequest x → toForeign x
    FormURLEncodedRequest x → toForeign (FormURLEncoded.encode x)
    JsonRequest x → toForeign (J.stringify x)

  responseSettings :: Tuple (Maybe MediaType) (ResponseType b)
  responseSettings = responseType

  headers :: Maybe RequestContent -> Array RequestHeader
  headers reqContent =
    addHeader (ContentType <$> (defaultMediaType =<< reqContent)) $
      addHeader (Accept <$> fst responseSettings)
        req.headers

  addHeader :: Maybe RequestHeader -> Array RequestHeader -> Array RequestHeader
  addHeader mh hs = case mh of
    Just h | not $ any (on eq requestHeaderName h) hs -> hs `Arr.snoc` h
    _ -> hs

  parseJSON :: String -> F Foreign
  parseJSON = either (fail <<< JSONError) (pure <<< toForeign) <<< jsonParser

  fromResponse' :: ResponseContent -> F b
  fromResponse' = case snd responseSettings of
    JSONResponse -> fromResponse <=< parseJSON <=< readString
    _ -> fromResponse

type AjaxRequest =
  { method :: String
  , url :: URL
  , headers :: Array { field :: String, value :: String }
  , content :: Nullable Foreign
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  , withCredentials :: Boolean
  }

foreign import _ajax :: forall e. Fn2 (String -> String -> ResponseHeader) AjaxRequest (AC.EffFnAff (ajax :: AJAX | e) (AffjaxResponse Foreign))
