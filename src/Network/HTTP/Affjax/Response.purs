module Network.HTTP.Affjax.Response
  ( ResponseType(..), responseTypeToString
  , ResponseContent()
  , Respondable, responseType, fromResponse
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foreign (Foreign(), F(), readString, unsafeReadTagged)
import DOM.File.Types (Blob())
import DOM.Node.Types (Document())
import DOM.XHR.Types (FormData())
import qualified Data.ArrayBuffer.Types as A

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
-- | (Blob, Document, JSON, String).
type ResponseContent = Foreign

class Respondable a where
  responseType :: ResponseType a
  fromResponse :: ResponseContent -> F a

instance responsableBlob :: Respondable Blob where
  responseType = BlobResponse
  fromResponse = unsafeReadTagged "Blob"

instance responsableDocument :: Respondable Document where
  responseType = DocumentResponse
  fromResponse = unsafeReadTagged "Document"

instance responsableJSON :: Respondable Foreign where
  responseType = JSONResponse
  fromResponse = Right

instance responsableString :: Respondable String where
  responseType = StringResponse
  fromResponse = readString

instance responsableUnit :: Respondable Unit where
  responseType = StringResponse
  fromResponse = const (Right unit)
