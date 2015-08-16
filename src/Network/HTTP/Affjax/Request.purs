module Network.HTTP.Affjax.Request
  ( RequestContent()
  , Requestable, toRequest
  ) where

import Prelude
import DOM.File.Types (Blob())
import DOM.Node.Types (Document())
import DOM.XHR.Types (FormData())
import qualified Unsafe.Coerce as U
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
  toRequest = U.unsafeCoerce

instance requestableInt16Array :: Requestable (A.ArrayView A.Int16) where
  toRequest = U.unsafeCoerce

instance requestableInt32Array :: Requestable (A.ArrayView A.Int32) where
  toRequest = U.unsafeCoerce

instance requestableUint8Array :: Requestable (A.ArrayView A.Uint8) where
  toRequest = U.unsafeCoerce

instance requestableUint16Array :: Requestable (A.ArrayView A.Uint16) where
  toRequest = U.unsafeCoerce

instance requestableUint32Array :: Requestable (A.ArrayView A.Uint32) where
  toRequest = U.unsafeCoerce

instance requestableUint8ClampedArray :: Requestable (A.ArrayView A.Uint8Clamped) where
  toRequest = U.unsafeCoerce

instance requestableFloat32Array :: Requestable (A.ArrayView A.Float32) where
  toRequest = U.unsafeCoerce

instance requestableFloat64Array :: Requestable (A.ArrayView A.Float64) where
  toRequest = U.unsafeCoerce

instance requestableBlob :: Requestable Blob where
  toRequest = U.unsafeCoerce

instance requestableDocument :: Requestable Document where
  toRequest = U.unsafeCoerce

instance requestableString :: Requestable String where
  toRequest = U.unsafeCoerce

instance requestableFormData :: Requestable FormData where
  toRequest = U.unsafeCoerce

instance requestableUnit :: Requestable Unit where
  toRequest = U.unsafeCoerce
