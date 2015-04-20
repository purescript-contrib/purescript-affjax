module Network.HTTP.Affjax.Response
  ( ResponseContent()
  , Respondable, responseType, fromResponse
  ) where

import Control.Bind ((>=>))
import Data.Either (Either(..))
import Data.Foreign (Foreign(), F(), readString, parseJSON, unsafeReadTagged)
import DOM (Document())
import DOM.File (Blob())
import DOM.XHR (FormData())
import Network.HTTP.Affjax.ResponseType
import Type.Proxy (Proxy())
import qualified Data.ArrayBuffer.Types as A

-- | Type representing content types that be received from an XHR request
-- | (Blob, Document, JSON, String).
type ResponseContent = Foreign

class Respondable a where
  responseType :: Proxy a -> ResponseType
  fromResponse :: ResponseContent -> F a

instance responsableBlob :: Respondable Blob where
  responseType _ = BlobResponse
  fromResponse = unsafeReadTagged "Blob"

instance responsableDocument :: Respondable Document where
  responseType _ = DocumentResponse
  fromResponse = unsafeReadTagged "Document"

instance responsableJSON :: Respondable Foreign where
  responseType _ = JSONResponse
  fromResponse = Right

instance responsableString :: Respondable String where
  responseType _ = StringResponse
  fromResponse = readString

instance responsableUnit :: Respondable Unit where
  responseType _ = StringResponse
  fromResponse = const (Right unit)
