module Network.Affjax where

import Control.Monad.Aff
import Network.Affjax.DSL
import Network.Affjax.Request
import Data.ArrayBuffer.Types

runAffjax :: forall e c a. AffjaxRequest c a -> Aff (ajax :: Ajax | e) AjaxResponse
runAffjax = ajax <<< affjaxRequest
