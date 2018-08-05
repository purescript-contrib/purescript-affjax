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

-- | Represents data for an HTTP request that will be included in the request
-- | body.
data Request
  = ArrayView (forall r. (forall a. A.ArrayView a -> r) -> r)
  | Blob Blob
  | Document Document
  | String String
  | FormData FormData
  | FormURLEncoded FormURLEncoded
  | Json Json

arrayView :: forall a. A.ArrayView a -> Request
arrayView av = ArrayView \f -> f av

blob :: Blob -> Request
blob = Blob

document :: Document -> Request
document = Document

string :: String -> Request
string = String

formData :: FormData -> Request
formData = FormData

formURLEncoded :: FormURLEncoded -> Request
formURLEncoded = FormURLEncoded

json :: Json -> Request
json = Json

toMediaType :: Request -> Maybe MediaType
toMediaType = case _ of
  FormURLEncoded _ -> Just applicationFormURLEncoded
  Json _ -> Just applicationJSON
  _ -> Nothing
