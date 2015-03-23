module Network.HTTP.Affjax.Response
  ( ResponseContent()
  , Responsable, toResponseType, fromContent
  ) where

import Data.Either (Either(..))
import Data.Foreign (Foreign(), ForeignError())
import Data.Options (IsOption, optionFn, (:=))
import Data.Proxy (Proxy())
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.HTTP.Affjax.ResponseType
import qualified Data.ArrayBuffer.Types as A

-- | Type representing content types that be received from an XHR request
-- | (ArrayBuffer, Blob, Document, JSON, String).
type ResponseContent = Foreign

-- | Class for types that converted from values returned from an XHR request.
class Responsable a where
  toResponseType :: Proxy a -> ResponseType
  fromContent :: ResponseContent -> Either ForeignError a

instance responsableUnit :: Responsable Unit where
  toResponseType _ = StringResponse
  fromContent _ = Right unit

instance responsableInt8Array :: Responsable (A.ArrayView A.Int8) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableInt16Array :: Responsable (A.ArrayView A.Int16) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableInt32Array :: Responsable (A.ArrayView A.Int32) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableUint8Array :: Responsable (A.ArrayView A.Uint8) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableUint16Array :: Responsable (A.ArrayView A.Uint16) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableUint32Array :: Responsable (A.ArrayView A.Uint32) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableUint8ClampedArray :: Responsable (A.ArrayView A.Uint8Clamped) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableFloat32Array :: Responsable (A.ArrayView A.Float32) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableFloat64Array :: Responsable (A.ArrayView A.Float64) where
  toResponseType _ = ArrayBufferResponse
  fromContent = arrayBufferConversion

instance responsableBlob :: Responsable Blob where
  toResponseType _ = BlobResponse
  fromContent = unsafeConversion

instance responsableDocument :: Responsable Document where
  toResponseType _ = DocumentResponse
  fromContent = unsafeConversion

instance responsableString :: Responsable String where
  toResponseType _ = StringResponse
  fromContent = Right <<< unsafeConversion

-- TODO: this, properly
foreign import arrayBufferConversion
  """
  function arrayBufferConversion (x) {
    return x;
  }
  """ :: forall a b. a -> b

-- TODO: not this either, at least use foreign to check the tag of returned values to ensure they are not null, etc.
foreign import unsafeConversion
  """
  function unsafeConversion (x) {
    return x;
  }
  """ :: forall a b. a -> b
