module Network.HTTP.ResponseHeader
  ( ResponseHeader()
  , responseHeader
  ) where

import Prelude

data ResponseHeader = ResponseHeader String String

responseHeader :: String -> String -> ResponseHeader
responseHeader field value = ResponseHeader field value

instance eqResponseHeader :: Eq ResponseHeader where
  eq (ResponseHeader h1 v1) (ResponseHeader h2 v2) = h1 == h2 && v1 == v2
  eq _ _ = false

instance showResponseHeader :: Show ResponseHeader where
  show (ResponseHeader h v) = "(ResponseHeader " ++ show h ++ " " ++ show v ++ ")"
