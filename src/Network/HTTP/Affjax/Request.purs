module Network.HTTP.Affjax.Request where

import DOM.File.Types (Blob)
import DOM.Node.Types (Document)
import DOM.XHR.Types (FormData)
import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types as A
import Data.FormURLEncoded (FormURLEncoded)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, applicationFormURLEncoded)

data RequestContent
  = ArrayViewInt8Request (A.ArrayView A.Int8)
  | ArrayViewInt16Request (A.ArrayView A.Int16)
  | ArrayViewInt32Request (A.ArrayView A.Int32)
  | ArrayViewUint8Request (A.ArrayView A.Uint8)
  | ArrayViewUint16Request (A.ArrayView A.Uint16)
  | ArrayViewUint32Request (A.ArrayView A.Uint32)
  | ArrayViewUint8ClampedRequest (A.ArrayView A.Uint8Clamped)
  | ArrayViewFloat32Request (A.ArrayView A.Float32)
  | ArrayViewFloat64Request (A.ArrayView A.Float64)
  | BlobRequest Blob
  | DocumentRequest Document
  | StringRequest String
  | FormDataRequest FormData
  | FormURLEncodedRequest FormURLEncoded
  | JsonRequest Json

defaultMediaType :: RequestContent -> Maybe MediaType
defaultMediaType = case _ of
  FormURLEncodedRequest _ -> Just applicationFormURLEncoded
  JsonRequest _ -> Just applicationJSON
  _ -> Nothing
