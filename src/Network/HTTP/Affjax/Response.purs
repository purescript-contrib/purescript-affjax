module Network.HTTP.Affjax.Response where

import Prelude

import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)

data AffjaxResponse a
  = ArrayBufferResponse (forall f. f ArrayBuffer -> f a)
  | BlobResponse (forall f. f Blob -> f a)
  | DocumentResponse (forall f. f Document -> f a)
  | JSONResponse (forall f. f Json -> f a)
  | StringResponse (forall f. f String -> f a)
  | IgnoredResponse (forall f. f Unit -> f a)

arrayBuffer :: AffjaxResponse ArrayBuffer
arrayBuffer = ArrayBufferResponse identity

blob :: AffjaxResponse Blob
blob = BlobResponse identity

document :: AffjaxResponse Document
document = DocumentResponse identity

json :: AffjaxResponse Json
json = JSONResponse identity

string :: AffjaxResponse String
string = StringResponse identity

ignored :: AffjaxResponse Unit
ignored = IgnoredResponse identity

responseTypeToString :: forall a. AffjaxResponse a -> String
responseTypeToString =
  case _ of
    ArrayBufferResponse _ -> "arraybuffer"
    BlobResponse _ -> "blob"
    DocumentResponse _ -> "document"
    JSONResponse _ -> "text" -- IE doesn't support "json" responseType
    StringResponse _ -> "text"
    IgnoredResponse _ -> "text"

responseTypeToMediaType :: forall a. AffjaxResponse a -> Maybe MediaType
responseTypeToMediaType =
  case _ of
    JSONResponse _ -> Just applicationJSON
    _ -> Nothing
