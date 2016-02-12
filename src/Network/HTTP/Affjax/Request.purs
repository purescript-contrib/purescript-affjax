module Network.HTTP.Affjax.Request
  ( RequestContent()
  , Requestable, toRequest
  , FormDataValue(..), toFormData
  , FormDataURLEncoded(..), toFormDataURLEncoded
  ) where

import Prelude

import Data.Argonaut.Core (Json())
import qualified Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import qualified Data.ArrayBuffer.Types as A
import Data.FormURLEncoded (FormURLEncoded())
import qualified Data.FormURLEncoded as URLEncoded

import DOM.File.Types (Blob())
import DOM.Node.Types (Document())
import DOM.XHR.Types (FormData())

import Global (encodeURIComponent)

import qualified Unsafe.Coerce as U

import Network.HTTP.MimeType (MimeType())
import Network.HTTP.MimeType.Common (applicationJSON, applicationFormURLEncoded)

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

instance requestableFormURLEncoded :: Requestable FormURLEncoded where
  toRequest form = Tuple (Just applicationFormURLEncoded)
                         (U.unsafeCoerce (URLEncoded.encode form))

instance requestableUnit :: Requestable Unit where
  toRequest = defaultToRequest

-- Helper functions to create `FormData` and URI encoded data (`FormDataURLEncoded`).

-- | Used to create values in `FormData`.
data FormDataValue = 
    FormDataString String
    -- TODO: Could add File, Blob, etc.

-- | Convert a mapping of keys to values into a `FormData`.
toFormData :: Array (Tuple String FormDataValue) -> FormData
toFormData dat =
  let form = newFormData unit in
  let _unit = map (appendData form) dat in
  form

  where
    appendData form (Tuple key (FormDataString val)) = appendString form key val

foreign import newFormData :: Unit -> FormData
foreign import appendString :: FormData -> String -> String -> Unit

-- | Newtype wrapper around a URL encoded string.
newtype FormDataURLEncoded = FormDataURLEncoded String 

-- | Convert a mapping of keys to `String` values into a URL encoded string (`FormDataURLEncoded`).
toFormDataURLEncoded :: Array (Tuple String String) -> FormDataURLEncoded
toFormDataURLEncoded dat =
  let encoded = map encode dat in
  FormDataURLEncoded $ Foldable.intercalate "&" encoded

  where
    encode :: (Tuple String String) -> String
    encode (Tuple key val) =
      encodeURIComponent key <> "=" <> encodeURIComponent val

instance formDataURLEncodedRequestable :: Requestable FormDataURLEncoded where
  toRequest (FormDataURLEncoded d) =
    Tuple (Just applicationFormURLEncoded) (U.unsafeCoerce d)
