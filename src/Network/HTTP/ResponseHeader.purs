module Network.HTTP.ResponseHeader where

data ResponseHeader = ResponseHeader String String

instance eqResponseHeader :: Eq ResponseHeader where
  (==) (ResponseHeader h1 v1) (ResponseHeader h2 v2) = h1 == h2 && v1 == v2
  (/=) x y = not (x == y)

instance showResponseHeader :: Show ResponseHeader where
  show (ResponseHeader h v) = "(ResponseHeader " ++ show h ++ " " ++ show v ++ ")"

responseHeaderName :: ResponseHeader -> String
responseHeaderName (ResponseHeader h _) = h

responseHeaderValue :: ResponseHeader -> String
responseHeaderValue (ResponseHeader _ v) = v
