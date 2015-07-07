module Network.HTTP.StatusCode where

import Prelude

import Data.Int

newtype StatusCode = StatusCode Int

instance eqStatusCode :: Eq StatusCode where
  eq (StatusCode x) (StatusCode y) = x == y

instance showStatusCode :: Show StatusCode where
  show (StatusCode code) = "(StatusCode " ++ show code ++ ")"
