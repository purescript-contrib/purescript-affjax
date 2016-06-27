module Network.HTTP.StatusCode where

import Prelude
import Data.Generic (class Generic)

newtype StatusCode = StatusCode Int

derive instance genericStatusCode :: Generic StatusCode

instance eqStatusCode :: Eq StatusCode where
  eq (StatusCode x) (StatusCode y) = x == y

instance showStatusCode :: Show StatusCode where
  show (StatusCode code) = "(StatusCode " <> show code <> ")"
