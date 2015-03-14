module Network.Affjax.DSL
  ( AffjaxRequest()
  , AffjaxRequestF(..)
  , affjaxRequest
  , url
  , method
  , method'
  , header
  , header'
  , content
  , content'
  , username
  , username'
  , password
  , password'
  ) where

import Control.Monad.Free (FreeC(), liftFC, runFreeCM)
import Control.Monad.State (State(), execState)
import Control.Monad.State.Class (modify)
import Data.Coyoneda (Natural())
import Data.Maybe (Maybe(..))
import Network.Affjax.Request
import Network.HTTP (Verb(..), Header(..), HeaderHead())

-- | A free monad for building AJAX requests
type AffjaxRequest = FreeC AffjaxRequestF

-- | The request DSL AST.
data AffjaxRequestF a
  = SetURL String a
  | SetMethod MethodName a
  | AddHeader Header a
  | SetContent (Maybe Content) a
  | SetUsername (Maybe String) a
  | SetPassword (Maybe String) a

-- | The interpreter for the request DSL AST.
affjaxN :: Natural AffjaxRequestF (State AjaxRequest)
affjaxN (SetURL url a) = const a <$> modify (_ { url = url })
affjaxN (SetMethod method a) = const a <$> modify (_ { method = method })
affjaxN (AddHeader header a) = const a <$> modify (\req -> req { headers = header : req.headers })
affjaxN (SetContent content a) = const a <$> modify (_ { content = content })
affjaxN (SetUsername username a) = const a <$> modify (_ { username = username })
affjaxN (SetPassword password a) = const a <$> modify (_ { password = password })

-- | Runs the DSL, producing an `AjaxRequest` object.
affjaxRequest :: forall a. AffjaxRequest a -> AjaxRequest
affjaxRequest = (`execState` defaultRequest) <<< runFreeCM affjaxN

-- | Take a standard HTTP verb and allow it to be used as an AJAX request
-- | method.
methodNameFromVerb :: Verb -> MethodName
methodNameFromVerb = MethodName <<< show

-- | Sets the URL for a request.
url :: String -> AffjaxRequest Unit
url url = liftFC (SetURL url unit)

-- | Sets the request method based on an HTTP verb.
method :: Verb -> AffjaxRequest Unit
method = method' <<< methodNameFromVerb

-- | Sets the request method.
method' :: MethodName -> AffjaxRequest Unit
method' meth = liftFC (SetMethod meth unit)

-- | Adds a header to the request using a key and value.
header :: HeaderHead -> String -> AffjaxRequest Unit
header key value = header' (Header key value)

-- | Adds a header to the request using a `Header` record.
header' :: Header -> AffjaxRequest Unit
header' header = liftFC (AddHeader header unit)

-- | Sets the content for the request.
content :: Content -> AffjaxRequest Unit
content value = content' (Just value)

-- | Sets the content for the request, with the option of setting it to
-- | `Nothing`.
content' :: Maybe Content -> AffjaxRequest Unit
content' value = liftFC (SetContent value unit)

-- | Sets the username for the request.
username :: String -> AffjaxRequest Unit
username value = username' (Just value)

-- | Sets the username for the request, with the option of setting it to
-- | `Nothing`.
username' :: Maybe String -> AffjaxRequest Unit
username' value = liftFC (SetUsername value unit)

-- | Sets the password for the request.
password :: String -> AffjaxRequest Unit
password value = password' (Just value)

-- | Sets the password for the request, with the option of setting it to
-- | `Nothing`.
password' :: Maybe String -> AffjaxRequest Unit
password' value = liftFC (SetPassword value unit)
