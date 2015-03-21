module Network.Affjax.Requestable
  ( AjaxRequestable, requestMimeType, toContent
  ) where

import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.Affjax.Request
import Network.HTTP.MimeType (MimeType())
import Network.HTTP.MimeType.Common (applicationOctetStream, multipartFormData, textHTML, textPlain)
import qualified Data.ArrayBuffer.Types as A

class AjaxRequestable a where
  requestMimeType :: a -> MimeType
  toContent :: a -> AjaxContent

instance requestableInt8Array :: AjaxRequestable (A.ArrayView A.Int8) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableInt16Array :: AjaxRequestable (A.ArrayView A.Int16) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableInt32Array :: AjaxRequestable (A.ArrayView A.Int32) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableUint8Array :: AjaxRequestable (A.ArrayView A.Uint8) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableUint16Array :: AjaxRequestable (A.ArrayView A.Uint16) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableUint32Array :: AjaxRequestable (A.ArrayView A.Uint32) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableUint8ClampedArray :: AjaxRequestable (A.ArrayView A.Uint8Clamped) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableFloat32Array :: AjaxRequestable (A.ArrayView A.Float32) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableFloat64Array :: AjaxRequestable (A.ArrayView A.Float64) where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableBlob :: AjaxRequestable Blob where
  requestMimeType = const applicationOctetStream
  toContent = unsafeToContent

instance requestableDocument :: AjaxRequestable Document where
  requestMimeType = const textHTML
  toContent = unsafeToContent

instance requestableString :: AjaxRequestable String where
  requestMimeType = const textPlain
  toContent = unsafeToContent

instance requestableFormData :: AjaxRequestable FormData where
  requestMimeType = const multipartFormData
  toContent = unsafeToContent

foreign import unsafeToContent
  """
  function unsafeToContent (x) {
    return x;
  }
  """ :: forall a. a -> AjaxContent
