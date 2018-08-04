module Network.HTTP.Affjax.Response where

import Prelude

import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)

-- | Used to represent how a HTTP response body should be interpreted.
data Response a
  = ArrayBuffer (forall f. f ArrayBuffer -> f a)
  | Blob (forall f. f Blob -> f a)
  | Document (forall f. f Document -> f a)
  | Json (forall f. f Json -> f a)
  | String (forall f. f String -> f a)
  | Ignore (forall f. f Unit -> f a)

arrayBuffer :: Response ArrayBuffer
arrayBuffer = ArrayBuffer identity

blob :: Response Blob
blob = Blob identity

document :: Response Document
document = Document identity

json :: Response Json
json = Json identity

string :: Response String
string = String identity

ignore :: Response Unit
ignore = Ignore identity

-- | Converts a `Response a` into a string representation of the response type
-- | that it represents.
toResponseType :: forall a. Response a -> String
toResponseType =
  case _ of
    ArrayBuffer _ -> "arraybuffer"
    Blob _ -> "blob"
    Document _ -> "document"
    Json _ -> "text" -- IE doesn't support "json" responseType
    String _ -> "text"
    Ignore _ -> ""

toMediaType :: forall a. Response a -> Maybe MediaType
toMediaType =
  case _ of
    Json _ -> Just applicationJSON
    _ -> Nothing
