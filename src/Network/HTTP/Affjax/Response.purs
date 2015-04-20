module Network.HTTP.Affjax.Response
  ( ResponseType(), responseTypeToString
  , ResponseContent()
  , Respondable, responseType, fromResponse
  ) where

import Control.Bind ((>=>))
import Data.Either (Either(..))
import Data.Foreign (Foreign(), F(), readString, parseJSON, unsafeReadTagged)
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Type.Proxy (Proxy())
import qualified Data.ArrayBuffer.Types as A

-- | Valid response types for an AJAX request. This is used to determine the
-- | `ResponseContent` type for a request.
data ResponseType
  = ArrayBufferResponse
  | BlobResponse
  | DocumentResponse
  | JSONResponse
  | StringResponse

instance eqResponseType :: Eq ResponseType where
  (==) ArrayBufferResponse ArrayBufferResponse = true
  (==) BlobResponse        BlobResponse        = true
  (==) DocumentResponse    DocumentResponse    = true
  (==) JSONResponse        JSONResponse        = true
  (==) StringResponse      StringResponse      = true
  (==) _ _ = false
  (/=) x y = not (x == y)

instance showResponseType :: Show ResponseType where
  show ArrayBufferResponse = "ArrayBufferResponse"
  show BlobResponse = "BlobResponse"
  show DocumentResponse = "DocumentResponse"
  show JSONResponse = "JSONResponse"
  show StringResponse = "StringResponse"

responseTypeToString :: ResponseType -> String
responseTypeToString ArrayBufferResponse = "arraybuffer"
responseTypeToString BlobResponse = "blob"
responseTypeToString DocumentResponse = "document"
responseTypeToString JSONResponse = "json"
responseTypeToString StringResponse = "text"

-- | Type representing content types that be received from an XHR request
-- | (Blob, Document, JSON, String).
type ResponseContent = Foreign

class Respondable a where
  responseType :: Proxy a -> ResponseType
  fromResponse :: ResponseContent -> F a

instance responsableBlob :: Respondable Blob where
  responseType _ = BlobResponse
  fromResponse = unsafeReadTagged "Blob"

instance responsableDocument :: Respondable Document where
  responseType _ = DocumentResponse
  fromResponse = unsafeReadTagged "Document"

instance responsableJSON :: Respondable Foreign where
  responseType _ = JSONResponse
  fromResponse = Right

instance responsableString :: Respondable String where
  responseType _ = StringResponse
  fromResponse = readString

instance responsableUnit :: Respondable Unit where
  responseType _ = StringResponse
  fromResponse = const (Right unit)
