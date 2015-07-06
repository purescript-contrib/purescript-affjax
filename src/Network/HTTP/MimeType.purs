module Network.HTTP.MimeType where

import Prelude

newtype MimeType = MimeType String

instance eqMimeType :: Eq MimeType where
  eq (MimeType x) (MimeType y) = x == y

instance showMimeType :: Show MimeType where
  show (MimeType h) = "(MimeType " ++ show h ++ ")"

mimeTypeToString :: MimeType -> String
mimeTypeToString (MimeType s) = s
