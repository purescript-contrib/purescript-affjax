module Network.HTTP.Affjax.ResponseType where

import Data.Options (IsOption, optionFn, (:=))

-- | Valid response types for an AJAX request. This is used to determine the
-- | `ResponseContent` type for a request.
data ResponseType
  = ArrayBufferResponse
  | BlobResponse
  | DocumentResponse
  | JSONResponse
  | StringResponse

instance eqResponseType :: Eq ResponseType where
  (==) ArrayBufferResponse ArrayBufferResponse = true
  (==) BlobResponse        BlobResponse        = true
  (==) DocumentResponse    DocumentResponse    = true
  (==) JSONResponse        JSONResponse        = true
  (==) StringResponse      StringResponse      = true
  (==) _ _ = false
  (/=) x y = not (x == y)

instance showResponseType :: Show ResponseType where
  show ArrayBufferResponse = "ArrayBufferResponse"
  show BlobResponse = "BlobResponse"
  show DocumentResponse = "DocumentResponse"
  show JSONResponse = "JSONResponse"
  show StringResponse = "StringResponse"

instance isOptionResponseType :: IsOption ResponseType where
  (:=) k a = (optionFn k) := ajaxResponseTypeToString a

ajaxResponseTypeToString :: ResponseType -> String
ajaxResponseTypeToString ArrayBufferResponse = "arraybuffer"
ajaxResponseTypeToString BlobResponse = "blob"
ajaxResponseTypeToString DocumentResponse = "document"
ajaxResponseTypeToString JSONResponse = "json"
ajaxResponseTypeToString StringResponse = "text"
