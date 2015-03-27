module Network.HTTP.Affjax.Response
  ( ResponseContent()
  , Responsable(..)
  , rInt8Array
  , rInt16Array
  , rInt32Array
  , rUint8Array
  , rUint16Array
  , rUint32Array
  , rUint8ClampedArray
  , rFloat32Array
  , rFloat64Array
  , rBlob
  , rDocument
  , rJSON
  , rString
  , rUnit
  ) where

import Data.Either (Either(..))
import Data.Foreign (Foreign(), F(), readString, unsafeReadTagged)
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.HTTP.Affjax.ResponseType
import qualified Data.ArrayBuffer.Types as A

-- | Type representing content types that be received from an XHR request
-- | (ArrayBuffer, Blob, Document, JSON, String).
type ResponseContent = Foreign

data Responsable a = Responsable (ResponseContent -> F a) ResponseType

rInt8Array :: Responsable A.Int8Array
rInt8Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rInt16Array :: Responsable A.Int16Array
rInt16Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rInt32Array :: Responsable A.Int32Array
rInt32Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rUint8Array :: Responsable A.Uint8Array
rUint8Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rUint16Array :: Responsable A.Uint16Array
rUint16Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rUint32Array :: Responsable A.Uint32Array
rUint32Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rUint8ClampedArray :: Responsable A.Uint8ClampedArray
rUint8ClampedArray = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rFloat32Array :: Responsable A.Float32Array
rFloat32Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rFloat64Array :: Responsable A.Float64Array
rFloat64Array = Responsable (unsafeReadTagged "ArrayBuffer") ArrayBufferResponse

rBlob :: Responsable Blob
rBlob = Responsable (unsafeReadTagged "Blob") BlobResponse

rDocument :: Responsable Document
rDocument = Responsable (unsafeReadTagged "Document") DocumentResponse

rJSON :: Responsable Foreign
rJSON = Responsable Right JSONResponse

rString :: Responsable String
rString = Responsable readString StringResponse

rUnit :: Responsable Unit
rUnit = Responsable (const $ Right unit) StringResponse
