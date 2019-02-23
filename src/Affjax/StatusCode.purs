module Affjax.StatusCode where

import Prelude

newtype StatusCode = StatusCode Int

derive instance eqStatusCode :: Eq StatusCode
derive instance ordStatusCode :: Ord StatusCode

instance showStatusCode :: Show StatusCode where
  show (StatusCode code) = "(StatusCode " <> show code <> ")"
