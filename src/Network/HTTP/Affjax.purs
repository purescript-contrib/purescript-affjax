module Network.HTTP.Affjax
  ( Affjax
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
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Parallel (parOneOf)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array (intercalate)
import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.FormURLEncoded as FormURLEncoded
import Data.Function (on)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.HTTP.Method as Method
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, try, delay)
import Effect.Aff.Compat as AC
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Foreign (F, Foreign, ForeignError(..), fail, renderForeignError, unsafeReadTagged, unsafeToForeign)
import Math as Math
import Network.HTTP.Affjax.Request as Request
import Network.HTTP.Affjax.Response as Response
import Network.HTTP.RequestHeader (RequestHeader(..), requestHeaderName, requestHeaderValue)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeader)
import Network.HTTP.StatusCode (StatusCode(..))

-- | The result type for Affjax requests.
type Affjax a = Aff (AffjaxResponse a)

type AffjaxRequest =
  { method :: Either Method CustomMethod
  , url :: URL
  , headers :: Array RequestHeader
  , content :: Maybe Request.Request
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
get :: forall a. Response.Response a -> URL -> Affjax a
get rt u = affjax rt $ defaultRequest { url = u }

-- | Makes a `POST` request to the specified URL, sending data.
post :: forall a. Response.Response a -> URL -> Request.Request -> Affjax a
post rt u c = affjax rt $ defaultRequest { method = Left POST, url = u, content = Just c }

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall a. Response.Response a -> URL -> Maybe Request.Request -> Affjax a
post' rt u c = affjax rt $ defaultRequest { method = Left POST, url = u, content = c }

-- | Makes a `POST` request to the specified URL, sending data and ignoring the
-- | response.
post_ :: URL -> Request.Request -> Affjax Unit
post_ = post Response.ignore

-- | Makes a `POST` request to the specified URL with the option to send data,
-- | and ignores the response.
post_' :: URL -> Maybe Request.Request -> Affjax Unit
post_' = post' Response.ignore

-- | Makes a `PUT` request to the specified URL, sending data.
put :: forall a. Response.Response a -> URL -> Request.Request -> Affjax a
put rt u c = affjax rt $ defaultRequest { method = Left PUT, url = u, content = Just c }

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall a. Response.Response a -> URL -> Maybe Request.Request -> Affjax a
put' rt u c = affjax rt $ defaultRequest { method = Left PUT, url = u, content = c }

-- | Makes a `PUT` request to the specified URL, sending data and ignoring the
-- | response.
put_ :: URL -> Request.Request -> Affjax Unit
put_ = put  Response.ignore

-- | Makes a `PUT` request to the specified URL with the option to send data,
-- | and ignores the response.
put_' :: URL -> Maybe Request.Request -> Affjax Unit
put_' = put'  Response.ignore

-- | Makes a `DELETE` request to the specified URL.
delete :: forall a. Response.Response a -> URL -> Affjax a
delete rt u = affjax rt $ defaultRequest { method = Left DELETE, url = u }

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: URL -> Affjax Unit
delete_ = delete Response.ignore

-- | Makes a `PATCH` request to the specified URL, sending data.
patch :: forall a. Response.Response a -> URL -> Request.Request -> Affjax a
patch rt u c = affjax rt $ defaultRequest { method = Left PATCH, url = u, content = Just c }

-- | Makes a `PATCH` request to the specified URL with the option to send data.
patch' :: forall a. Response.Response a -> URL -> Maybe Request.Request -> Affjax a
patch' rt u c = affjax rt $ defaultRequest { method = Left PATCH, url = u, content = c }

-- | Makes a `PATCH` request to the specified URL, sending data and ignoring the
-- | response.
patch_ :: URL -> Request.Request -> Affjax Unit
patch_ = patch Response.ignore

-- | Makes a `PATCH` request to the specified URL with the option to send data,
-- | and ignores the response.
patch_' :: URL -> Maybe Request.Request -> Affjax Unit
patch_' = patch' Response.ignore

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
retry :: forall a. RetryPolicy -> (AffjaxRequest -> Affjax a) -> AffjaxRequest -> Affjax a
retry policy run req = do
  -- failureRef is either an exception or a failed request
  failureRef <- liftEffect $ Ref.new Nothing
  let loop = go failureRef
  case policy.timeout of
    Nothing -> loop 1
    Just timeout -> do
      result <- parOneOf [ Just <$> loop 1, Nothing <$ delay timeout ]
      case result of
        Nothing -> do
          failure <- liftEffect $ Ref.read failureRef
          case failure of
            Nothing -> throwError $ error "Timeout"
            Just failure' -> either throwError pure failure'
        Just resp -> pure resp
  where
    retryState
      :: Either Error (AffjaxResponse a)
      -> RetryState Error (AffjaxResponse a)
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
          liftEffect $ Ref.write (Just err) failureRef
          delay (policy.delayCurve n)
          go failureRef (n + 1)
        Right resp -> pure resp

-- | Makes an `Affjax` request.
affjax :: forall a. Response.Response a -> AffjaxRequest -> Affjax a
affjax rt req = do
  res <- AC.fromEffectFnAff $ runFn2 _ajax responseHeader req'
  case runExcept (fromResponse' res.response) of
    Left err -> throwError $ error $ intercalate "\n" (map renderForeignError err)
    Right res' -> pure (res { response = res' })
  where

  req' :: AjaxRequest a
  req' =
    { method: Method.print req.method
    , url: req.url
    , headers: (\h -> { field: requestHeaderName h, value: requestHeaderValue h }) <$> headers req.content
    , content: toNullable (extractContent <$> req.content)
    , responseType: Response.toResponseType rt
    , username: toNullable req.username
    , password: toNullable req.password
    , withCredentials: req.withCredentials
    }

  extractContent :: Request.Request -> Foreign
  extractContent = case _ of
    Request.ArrayView f → f unsafeToForeign
    Request.Blob x → unsafeToForeign x
    Request.Document x → unsafeToForeign x
    Request.String x → unsafeToForeign x
    Request.FormData x → unsafeToForeign x
    Request.FormURLEncoded x → unsafeToForeign (FormURLEncoded.encode x)
    Request.Json x → unsafeToForeign (J.stringify x)

  headers :: Maybe Request.Request -> Array RequestHeader
  headers reqContent =
    addHeader (ContentType <$> (Request.toMediaType =<< reqContent)) $
      addHeader (Accept <$> Response.toMediaType rt)
        req.headers

  addHeader :: Maybe RequestHeader -> Array RequestHeader -> Array RequestHeader
  addHeader mh hs = case mh of
    Just h | not $ any (on eq requestHeaderName h) hs -> hs `Arr.snoc` h
    _ -> hs

  parseJSON :: String -> F Json
  parseJSON = case _ of
    "" -> pure J.jsonEmptyObject
    str -> either (fail <<< ForeignError) pure (jsonParser str)

  fromResponse' :: Foreign -> F a
  fromResponse' = case rt of
    Response.ArrayBuffer _ -> unsafeReadTagged "ArrayBuffer"
    Response.Blob _ -> unsafeReadTagged "Blob"
    Response.Document _ -> unsafeReadTagged "Document"
    Response.Json coe -> coe <<< parseJSON <=< unsafeReadTagged "String"
    Response.String _ -> unsafeReadTagged "String"
    Response.Ignore coe -> const $ coe (pure unit)

type AjaxRequest a =
  { method :: String
  , url :: URL
  , headers :: Array { field :: String, value :: String }
  , content :: Nullable Foreign
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  , withCredentials :: Boolean
  }

foreign import _ajax
  :: forall a
   . Fn2
      (String -> String -> ResponseHeader)
      (AjaxRequest a)
      (AC.EffectFnAff (AffjaxResponse Foreign))
