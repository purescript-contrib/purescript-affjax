module Network.HTTP.Affjax.Response
  ( ResponseType(..), responseTypeToString
  , ResponseContent
  , class Respondable, responseType, fromResponse
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types as A
import Data.Foreign (Foreign, F, readString, unsafeReadTagged)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))

import DOM.File.Types (Blob)
import DOM.Node.Types (Document)

import Unsafe.Coerce (unsafeCoerce)

-- | Valid response types for an AJAX request. This is used to determine the
-- | `ResponseContent` type for a request. The `a` type variable is a phantom
-- | type used to associate the `ResponseType` with a particular instance of
-- | `Respondable`.
data ResponseType a
  = ArrayBufferResponse
  | BlobResponse
  | DocumentResponse
  | JSONResponse
  | StringResponse

instance eqResponseType :: Eq (ResponseType a) where
  eq ArrayBufferResponse ArrayBufferResponse = true
  eq BlobResponse        BlobResponse        = true
  eq DocumentResponse    DocumentResponse    = true
  eq JSONResponse        JSONResponse        = true
  eq StringResponse      StringResponse      = true
  eq _ _ = false

instance showResponseType :: Show (ResponseType a) where
  show ArrayBufferResponse = "ArrayBufferResponse"
  show BlobResponse = "BlobResponse"
  show DocumentResponse = "DocumentResponse"
  show JSONResponse = "JSONResponse"
  show StringResponse = "StringResponse"

responseTypeToString :: forall a. (ResponseType a) -> String
responseTypeToString ArrayBufferResponse = "arraybuffer"
responseTypeToString BlobResponse = "blob"
responseTypeToString DocumentResponse = "document"
responseTypeToString JSONResponse = "text" -- IE doesn't support "json" responseType
responseTypeToString StringResponse = "text"

-- | Type representing content types that be received from an XHR request
-- | (Blob, Document, JSON, String). An optional mime-type can be specified for
-- | a default `Accept` header.
type ResponseContent = Foreign

class Respondable a where
  responseType :: Tuple (Maybe MediaType) (ResponseType a)
  fromResponse :: ResponseContent -> F a

instance responsableBlob :: Respondable Blob where
  responseType = Tuple Nothing BlobResponse
  fromResponse = unsafeReadTagged "Blob"

instance responsableDocument :: Respondable Document where
  responseType = Tuple Nothing DocumentResponse
  fromResponse = unsafeReadTagged "Document"

instance responsableForeign :: Respondable Foreign where
  responseType = Tuple Nothing JSONResponse
  fromResponse = pure <<< unsafeCoerce

instance responsableString :: Respondable String where
  responseType = Tuple Nothing StringResponse
  fromResponse = readString

instance responsableUnit :: Respondable Unit where
  responseType = Tuple Nothing StringResponse
  fromResponse = const (pure unit)

instance responsableArrayBuffer :: Respondable A.ArrayBuffer where
  responseType = Tuple Nothing ArrayBufferResponse
  fromResponse = unsafeReadTagged "ArrayBuffer"

instance responsableJson :: Respondable Json where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse = pure <<< unsafeCoerce
