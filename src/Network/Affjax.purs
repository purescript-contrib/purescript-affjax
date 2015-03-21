module Network.Affjax where

import Control.Monad.Aff
import Network.Affjax.DSL
import Network.Affjax.Request

runAffjax :: forall e a. AffjaxRequest a -> Aff (ajax :: Ajax | e) AjaxResponse
runAffjax = ajax <<< affjaxRequest
