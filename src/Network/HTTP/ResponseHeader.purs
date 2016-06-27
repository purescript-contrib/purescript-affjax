module Network.HTTP.ResponseHeader
  ( ResponseHeader()
  , responseHeader
  , responseHeaderName
  , responseHeaderValue
  ) where

import Prelude
import Data.Generic (class Generic)

data ResponseHeader = ResponseHeader String String

derive instance genericResponseHeader :: Generic ResponseHeader

responseHeader :: String -> String -> ResponseHeader
responseHeader field value = ResponseHeader field value

instance eqResponseHeader :: Eq ResponseHeader where
  eq (ResponseHeader h1 v1) (ResponseHeader h2 v2) = h1 == h2 && v1 == v2

instance showResponseHeader :: Show ResponseHeader where
  show (ResponseHeader h v) = "(ResponseHeader " <> show h <> " " <> show v <> ")"

responseHeaderName :: ResponseHeader -> String
responseHeaderName (ResponseHeader h _) = h

responseHeaderValue :: ResponseHeader -> String
responseHeaderValue (ResponseHeader _ v) = v
