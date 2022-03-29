module Affjax
  ( Request
  , defaultRequest
  , Response
  , Error(..)
  , printError
  , URL
  ) where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader)
import Affjax.StatusCode (StatusCode)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect.Exception as Exn
import Foreign (Foreign, ForeignError, renderForeignError)

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
  , timeout :: Maybe Milliseconds
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
  , timeout: Nothing
  }

-- | The possible errors that can occur when making an Affjax request.
data Error
  = RequestContentError String
  | ResponseBodyError ForeignError (Response Foreign)
  | TimeoutError
  | RequestFailedError
  | XHROtherError Exn.Error

printError :: Error -> String
printError = case _ of
  RequestContentError err ->
    "There was a problem with the request content: " <> err
  ResponseBodyError err _ ->
    "There was a problem with the response body: " <> renderForeignError err
  TimeoutError ->
    "There was a problem making the request: timeout"
  RequestFailedError ->
    "There was a problem making the request: request failed"
  XHROtherError err ->
    "There was a problem making the request: " <> Exn.message err

-- | The type of records that represents a received HTTP response.
type Response a =
  { status :: StatusCode
  , statusText :: String
  , headers :: Array ResponseHeader
  , body :: a
  }

-- | Type alias for URL strings to aid readability of types.
type URL = String
