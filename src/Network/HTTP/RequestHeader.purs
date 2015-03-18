module Network.HTTP.RequestHeader where

import Network.HTTP.MimeType

data RequestHeader
  = ContentType MimeType
  | RequestHeader String String

instance eqRequestHeader :: Eq RequestHeader where
  (==) (ContentType m1) (ContentType m2) = m1 == m2
  (==) (RequestHeader h1 v1) (RequestHeader h2 v2) = h1 == h2 && v1 == v2
  (/=) x y = not (x == y)

instance showRequestHeader :: Show RequestHeader where
  show (ContentType m) = "(ContentType " ++ show m ++ ")"
  show (RequestHeader h v) = "(RequestHeader " ++ show h ++ " " ++ show v ++ ")"

requestHeaderName :: RequestHeader -> String
requestHeaderName (ContentType _) = "ContentType"
requestHeaderName (RequestHeader h _) = h

requestHeaderValue :: RequestHeader -> String
requestHeaderValue (ContentType m) = mimeTypeToString m
requestHeaderValue (RequestHeader _ v) = v
