module Network.HTTP.Affjax.Request where

import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types as A
import Data.FormURLEncoded (FormURLEncoded)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, applicationFormURLEncoded)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)
import Web.XHR.FormData (FormData)

data RequestContent
  = ArrayView (forall r. (forall a. A.ArrayView a -> r) -> r)
  | BlobRequest Blob
  | DocumentRequest Document
  | StringRequest String
  | FormDataRequest FormData
  | FormURLEncodedRequest FormURLEncoded
  | JsonRequest Json

arrayView :: forall a. A.ArrayView a -> RequestContent
arrayView av = ArrayView \f -> f av

defaultMediaType :: RequestContent -> Maybe MediaType
defaultMediaType = case _ of
  FormURLEncodedRequest _ -> Just applicationFormURLEncoded
  JsonRequest _ -> Just applicationJSON
  _ -> Nothing
