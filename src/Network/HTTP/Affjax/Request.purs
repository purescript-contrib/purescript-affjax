module Network.HTTP.Affjax.Request
  ( RequestContent()
  , Requestable, toRequest
  ) where

import Prelude

import Data.Argonaut.Core (Json())
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import qualified Data.ArrayBuffer.Types as A

import DOM.File.Types (Blob())
import DOM.Node.Types (Document())
import DOM.XHR.Types (FormData())

import qualified Unsafe.Coerce as U

import Network.HTTP.MimeType (MimeType())
import Network.HTTP.MimeType.Common (applicationJSON)

-- | Type representing all content types that be sent via XHR (ArrayBufferView,
-- | Blob, Document, String, FormData).
foreign import data RequestContent :: *

-- | A class for types that can be converted to values that can be sent with
-- | XHR requests. An optional mime-type can be specified for a default
-- | `Content-Type` header.
class Requestable a where
  toRequest :: a -> Tuple (Maybe MimeType) RequestContent

defaultToRequest :: forall a. a -> Tuple (Maybe MimeType) RequestContent
defaultToRequest = Tuple Nothing <<< U.unsafeCoerce

instance requestableRequestContent :: Requestable RequestContent where
  toRequest = defaultToRequest

instance requestableInt8Array :: Requestable (A.ArrayView A.Int8) where
  toRequest = defaultToRequest

instance requestableInt16Array :: Requestable (A.ArrayView A.Int16) where
  toRequest = defaultToRequest

instance requestableInt32Array :: Requestable (A.ArrayView A.Int32) where
  toRequest = defaultToRequest

instance requestableUint8Array :: Requestable (A.ArrayView A.Uint8) where
  toRequest = defaultToRequest

instance requestableUint16Array :: Requestable (A.ArrayView A.Uint16) where
  toRequest = defaultToRequest

instance requestableUint32Array :: Requestable (A.ArrayView A.Uint32) where
  toRequest = defaultToRequest

instance requestableUint8ClampedArray :: Requestable (A.ArrayView A.Uint8Clamped) where
  toRequest = defaultToRequest

instance requestableFloat32Array :: Requestable (A.ArrayView A.Float32) where
  toRequest = defaultToRequest

instance requestableFloat64Array :: Requestable (A.ArrayView A.Float64) where
  toRequest = defaultToRequest

instance requestableBlob :: Requestable Blob where
  toRequest = defaultToRequest

instance requestableDocument :: Requestable Document where
  toRequest = defaultToRequest

instance requestableString :: Requestable String where
  toRequest = defaultToRequest

instance requestableJson :: Requestable Json where
  toRequest json = Tuple (Just applicationJSON) (U.unsafeCoerce (show json))

instance requestableFormData :: Requestable FormData where
  toRequest = defaultToRequest

instance requestableUnit :: Requestable Unit where
  toRequest = defaultToRequest
