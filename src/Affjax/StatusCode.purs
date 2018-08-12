module Affjax.StatusCode where

import Prelude

newtype StatusCode = StatusCode Int

instance eqStatusCode :: Eq StatusCode where
  eq (StatusCode x) (StatusCode y) = x == y

instance showStatusCode :: Show StatusCode where
  show (StatusCode code) = "(StatusCode " <> show code <> ")"
