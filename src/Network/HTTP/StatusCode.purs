module Network.HTTP.StatusCode where

import Data.Int

newtype StatusCode = StatusCode Int

instance eqStatusCode :: Eq StatusCode where
  (==) (StatusCode x) (StatusCode y) = x == y
  (/=) x y = not (x == y)

instance showStatusCode :: Show StatusCode where
  show (StatusCode code) = "(StatusCode " ++ show code ++ ")"
