module Affjax
  ( Request, defaultRequest
  , Response
  , URL
  , request
  , get
  , post, post_, post', post_'
  , put, put_, put', put_'
  , delete, delete_
  , patch, patch_, patch', patch_'
  , RetryDelayCurve
  , RetryPolicy(..)
  , defaultRetryPolicy
  , retry
  , module Affjax.ResponseFormat
  ) where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat (ResponseFormatError(..), printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (runExcept, throwError)
import Control.Parallel (parOneOf)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.FormURLEncoded as FormURLEncoded
import Data.Function (on)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.HTTP.Method as Method
import Data.Int (toNumber)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, try, delay)
import Effect.Aff.Compat as AC
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Foreign (F, Foreign, ForeignError(..), fail, unsafeReadTagged, unsafeToForeign)
import Math as Math

-- | A record that contains all the information to perform an HTTP request.
-- | Instead of constructing the record from scratch it is often easier to build
-- | one based on `defaultRequest`.
type Request a =
  { method :: Either Method CustomMethod
  , url :: URL
  , headers :: Array RequestHeader
  , content :: Maybe RequestBody.RequestBody
  , username :: Maybe String
  , password :: Maybe String
  , withCredentials :: Boolean
  , responseFormat :: ResponseFormat.ResponseFormat a
  }

-- | A record of the type `Request` that has all fields set to default
-- | values. This record can be used as the foundation for constructing
-- | custom requests.
-- |
-- | As an example:
-- |
-- | ```purescript
-- | defaultRequest { url = "/api/user", method = Left POST }
-- | ```
-- |
-- | Would represents a POST request to the URL `/api/user`.
defaultRequest :: Request Unit
defaultRequest =
  { method: Left GET
  , url: "/"
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: ResponseFormat.ignore
  }

-- | The type of records that represents a received HTTP response.
type Response a =
  { status :: StatusCode
  , statusText :: String
  , headers :: Array ResponseHeader
  , body :: a
  }

-- | Type alias for URL strings to aid readability of types.
type URL = String

-- | Makes a `GET` request to the specified URL.
get :: forall a. ResponseFormat.ResponseFormat a -> URL -> Aff (Response (Either ResponseFormatError a))
get rf u = request (defaultRequest { url = u, responseFormat = rf })

-- | Makes a `POST` request to the specified URL, sending data.
post :: forall a. ResponseFormat.ResponseFormat a -> URL -> RequestBody.RequestBody -> Aff (Response (Either ResponseFormatError a))
post rf u c = request (defaultRequest { method = Left POST, url = u, content = Just c, responseFormat = rf })

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall a. ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Response (Either ResponseFormatError a))
post' rf u c = request (defaultRequest { method = Left POST, url = u, content = c, responseFormat = rf })

-- | Makes a `POST` request to the specified URL, sending data and ignoring the
-- | response.
post_ :: URL -> RequestBody.RequestBody -> Aff (Response Unit)
post_ url = map (_ { body = unit }) <<< post ResponseFormat.ignore url

-- | Makes a `POST` request to the specified URL with the option to send data,
-- | and ignores the response.
post_' :: URL -> Maybe RequestBody.RequestBody -> Aff (Response Unit)
post_' url = map (_ { body = unit }) <<< post' ResponseFormat.ignore url

-- | Makes a `PUT` request to the specified URL, sending data.
put :: forall a. ResponseFormat.ResponseFormat a -> URL -> RequestBody.RequestBody -> Aff (Response (Either ResponseFormatError a))
put rf u c = request (defaultRequest { method = Left PUT, url = u, content = Just c, responseFormat = rf })

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall a. ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Response (Either ResponseFormatError a))
put' rf u c = request (defaultRequest { method = Left PUT, url = u, content = c, responseFormat = rf })

-- | Makes a `PUT` request to the specified URL, sending data and ignoring the
-- | response.
put_ :: URL -> RequestBody.RequestBody -> Aff (Response Unit)
put_ url = map (_ { body = unit }) <<< put ResponseFormat.ignore url

-- | Makes a `PUT` request to the specified URL with the option to send data,
-- | and ignores the response.
put_' :: URL -> Maybe RequestBody.RequestBody -> Aff (Response Unit)
put_' url = map (_ { body = unit }) <<< put' ResponseFormat.ignore url

-- | Makes a `DELETE` request to the specified URL.
delete :: forall a. ResponseFormat.ResponseFormat a -> URL -> Aff (Response (Either ResponseFormatError a))
delete rf u = request (defaultRequest { method = Left DELETE, url = u, responseFormat = rf })

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: URL -> Aff (Response Unit)
delete_ = map (_ { body = unit }) <<< delete ResponseFormat.ignore

-- | Makes a `PATCH` request to the specified URL, sending data.
patch :: forall a. ResponseFormat.ResponseFormat a -> URL -> RequestBody.RequestBody -> Aff (Response (Either ResponseFormatError a))
patch rf u c = request (defaultRequest { method = Left PATCH, url = u, content = Just c, responseFormat = rf })

-- | Makes a `PATCH` request to the specified URL with the option to send data.
patch' :: forall a. ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Response (Either ResponseFormatError a))
patch' rf u c = request (defaultRequest { method = Left PATCH, url = u, content = c, responseFormat = rf })

-- | Makes a `PATCH` request to the specified URL, sending data and ignoring the
-- | response.
patch_ :: URL -> RequestBody.RequestBody -> Aff (Response Unit)
patch_ url = map (_ { body = unit }) <<< patch ResponseFormat.ignore url

-- | Makes a `PATCH` request to the specified URL with the option to send data,
-- | and ignores the response.
patch_' :: URL -> Maybe RequestBody.RequestBody -> Aff (Response Unit)
patch_' url = map (_ { body = unit }) <<< patch' ResponseFormat.ignore url

-- | A sequence of retry delays, in milliseconds.
type RetryDelayCurve = Int -> Milliseconds

-- | Expresses a policy for retrying HTTP requests with backoff.
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
retry :: forall a b. RetryPolicy -> (Request a -> Aff (Response b)) -> Request a -> Aff (Response b)
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
      :: Either Error (Response b)
      -> RetryState Error (Response b)
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

-- | Makes an HTTP request.
-- |
-- | The example below performs a `GET` request to the URL `/resource` and
-- | interprets the response body as JSON.
-- |
-- | ```purescript
-- | import Affjax.ResponseFormat (json)
-- | ...
-- | request (defaultRequest { url = "/resource", method = Left GET, responseFormat = json})
-- | ```
-- |
-- | For common cases helper functions can often be used instead of `request` .
-- | For instance, the above example is equivalent to the following.
-- |
-- | ```purescript
-- | get json "/resource"
-- | ```
request :: forall a. Request a -> Aff (Response (Either ResponseFormatError a))
request req = do
  res <- AC.fromEffectFnAff $ runFn2 _ajax ResponseHeader req'
  case runExcept (fromResponse' res.body) of
    Left err -> do
      pure (res { body = Left (ResponseFormatError (NEL.head err) res.body) })
    Right res' -> do
      pure (res { body = Right res' })
  where

  req' :: AjaxRequest a
  req' =
    { method: Method.print req.method
    , url: req.url
    , headers: (\h -> { field: RequestHeader.name h, value: RequestHeader.value h }) <$> headers req.content
    , content: toNullable (extractContent <$> req.content)
    , responseType: ResponseFormat.toResponseType req.responseFormat
    , username: toNullable req.username
    , password: toNullable req.password
    , withCredentials: req.withCredentials
    }

  extractContent :: RequestBody.RequestBody -> Foreign
  extractContent = case _ of
    RequestBody.ArrayView f → f unsafeToForeign
    RequestBody.Blob x → unsafeToForeign x
    RequestBody.Document x → unsafeToForeign x
    RequestBody.String x → unsafeToForeign x
    RequestBody.FormData x → unsafeToForeign x
    RequestBody.FormURLEncoded x → unsafeToForeign (FormURLEncoded.encode x)
    RequestBody.Json x → unsafeToForeign (J.stringify x)

  headers :: Maybe RequestBody.RequestBody -> Array RequestHeader
  headers reqContent =
    addHeader (ContentType <$> (RequestBody.toMediaType =<< reqContent)) $
      addHeader (Accept <$> ResponseFormat.toMediaType req.responseFormat)
        req.headers

  addHeader :: Maybe RequestHeader -> Array RequestHeader -> Array RequestHeader
  addHeader mh hs = case mh of
    Just h | not $ any (on eq RequestHeader.name h) hs -> hs `Arr.snoc` h
    _ -> hs

  parseJSON :: String -> F Json
  parseJSON = case _ of
    "" -> pure J.jsonEmptyObject
    str -> either (fail <<< ForeignError) pure (jsonParser str)

  fromResponse' :: Foreign -> F a
  fromResponse' = case req.responseFormat of
    ResponseFormat.ArrayBuffer _ -> unsafeReadTagged "ArrayBuffer"
    ResponseFormat.Blob _ -> unsafeReadTagged "Blob"
    ResponseFormat.Document _ -> unsafeReadTagged "Document"
    ResponseFormat.Json coe -> coe <<< parseJSON <=< unsafeReadTagged "String"
    ResponseFormat.String _ -> unsafeReadTagged "String"
    ResponseFormat.Ignore coe -> const $ coe (pure unit)

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
      (AC.EffectFnAff (Response Foreign))
