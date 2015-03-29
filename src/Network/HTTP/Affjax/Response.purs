module Network.HTTP.Affjax.Response
  ( ResponseContent()
  , Responsable, responseType, fromResponse
  ) where

import Control.Bind ((>=>))
import Data.Either (Either(..))
import Data.Foreign (Foreign(), F(), readString, parseJSON, unsafeReadTagged)
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.HTTP.Affjax.ResponseType
import Type.Proxy (Proxy())
import qualified Data.ArrayBuffer.Types as A

-- | Type representing content types that be received from an XHR request
-- | (ArrayBuffer, Blob, Document, JSON, String).
type ResponseContent = Foreign

class Responsable a where
  responseType :: Proxy a -> ResponseType
  fromResponse :: ResponseContent -> F a

-- rInt8Array :: Responsable A.Int8Array
-- rInt8Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rInt16Array :: Responsable A.Int16Array
-- rInt16Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rInt32Array :: Responsable A.Int32Array
-- rInt32Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rUint8Array :: Responsable A.Uint8Array
-- rUint8Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rUint16Array :: Responsable A.Uint16Array
-- rUint16Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rUint32Array :: Responsable A.Uint32Array
-- rUint32Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rUint8ClampedArray :: Responsable A.Uint8ClampedArray
-- rUint8ClampedArray = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rFloat32Array :: Responsable A.Float32Array
-- rFloat32Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

-- rFloat64Array :: Responsable A.Float64Array
-- rFloat64Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

instance responsableBlob :: Responsable Blob where
  responseType _ = BlobResponse
  fromResponse = unsafeReadTagged "Blob"

instance responsableDocument :: Responsable Document where
  responseType _ = DocumentResponse
  fromResponse = unsafeReadTagged "Document"

instance responsableJSON :: Responsable Foreign where
  responseType _ = JSONResponse
  fromResponse = readString >=> parseJSON

instance responsableString :: Responsable String where
  responseType _ = StringResponse
  fromResponse = readString

instance responsableUnit :: Responsable Unit where
  responseType _ = StringResponse
  fromResponse = const (Right unit)
