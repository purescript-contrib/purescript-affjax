module Network.HTTP.Affjax.Request
  ( RequestContent()
  , Requestable, toContent
  ) where

import Data.Options (Option(), Options(), IsOption, optionFn, (:=))
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.HTTP.MimeType (MimeType())
import qualified Data.ArrayBuffer.Types as A

-- | Type representing all content types that be sent via XHR (ArrayBufferView,
-- | Blob, Document, String, FormData).
foreign import data RequestContent :: *

instance isOptionRequestContent :: IsOption RequestContent where
  (:=) = unsafeIsOption

-- | A class for types that can be converted to values that can be sent with
-- | XHR requests.
class Requestable a where
  toContent :: a -> RequestContent

instance requestableAjaxRequestContent :: Requestable RequestContent where
  toContent = id

instance requestableInt8Array :: Requestable (A.ArrayView A.Int8) where
  toContent = unsafeConversion

instance requestableInt16Array :: Requestable (A.ArrayView A.Int16) where
  toContent = unsafeConversion

instance requestableInt32Array :: Requestable (A.ArrayView A.Int32) where
  toContent = unsafeConversion

instance requestableUint8Array :: Requestable (A.ArrayView A.Uint8) where
  toContent = unsafeConversion

instance requestableUint16Array :: Requestable (A.ArrayView A.Uint16) where
  toContent = unsafeConversion

instance requestableUint32Array :: Requestable (A.ArrayView A.Uint32) where
  toContent = unsafeConversion

instance requestableUint8ClampedArray :: Requestable (A.ArrayView A.Uint8Clamped) where
  toContent = unsafeConversion

instance requestableFloat32Array :: Requestable (A.ArrayView A.Float32) where
  toContent = unsafeConversion

instance requestableFloat64Array :: Requestable (A.ArrayView A.Float64) where
  toContent = unsafeConversion

instance requestableBlob :: Requestable Blob where
  toContent = unsafeConversion

instance requestableDocument :: Requestable Document where
  toContent = unsafeConversion

instance requestableString :: Requestable String where
  toContent = unsafeConversion

instance requestableFormData :: Requestable FormData where
  toContent = unsafeConversion

instance requestableUnit :: Requestable Unit where
  toContent = unsafeConversion

foreign import unsafeIsOption
  """
  function unsafeIsOption(k) {
    return function (v) {
      return [[k, v]];
    };
  }
  """ :: forall b a. (Option b a) -> a -> (Options b)

foreign import unsafeConversion
  """
  function unsafeConversion (x) {
    return x;
  }
  """ :: forall a b. a -> b
