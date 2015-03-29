module Network.HTTP.Affjax.Request
  ( RequestContent()
  , Requestable, toRequest
  ) where

import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import qualified Data.ArrayBuffer.Types as A

-- | Type representing all content types that be sent via XHR (ArrayBufferView,
-- | Blob, Document, String, FormData).
foreign import data RequestContent :: *

-- | A class for types that can be converted to values that can be sent with
-- | XHR requests.
class Requestable a where
  toRequest :: a -> RequestContent

instance requestableRequestContent :: Requestable RequestContent where
  toRequest = id

instance requestableInt8Array :: Requestable (A.ArrayView A.Int8) where
  toRequest = unsafeConversion

instance requestableInt16Array :: Requestable (A.ArrayView A.Int16) where
  toRequest = unsafeConversion

instance requestableInt32Array :: Requestable (A.ArrayView A.Int32) where
  toRequest = unsafeConversion

instance requestableUint8Array :: Requestable (A.ArrayView A.Uint8) where
  toRequest = unsafeConversion

instance requestableUint16Array :: Requestable (A.ArrayView A.Uint16) where
  toRequest = unsafeConversion

instance requestableUint32Array :: Requestable (A.ArrayView A.Uint32) where
  toRequest = unsafeConversion

instance requestableUint8ClampedArray :: Requestable (A.ArrayView A.Uint8Clamped) where
  toRequest = unsafeConversion

instance requestableFloat32Array :: Requestable (A.ArrayView A.Float32) where
  toRequest = unsafeConversion

instance requestableFloat64Array :: Requestable (A.ArrayView A.Float64) where
  toRequest = unsafeConversion

instance requestableBlob :: Requestable Blob where
  toRequest = unsafeConversion

instance requestableDocument :: Requestable Document where
  toRequest = unsafeConversion

instance requestableString :: Requestable String where
  toRequest = unsafeConversion

instance requestableFormData :: Requestable FormData where
  toRequest = unsafeConversion

instance requestableUnit :: Requestable Unit where
  toRequest = unsafeConversion

foreign import unsafeConversion
  """
  function unsafeConversion (x) {
    return x;
  }
  """ :: forall a b. a -> b
