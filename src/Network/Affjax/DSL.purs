module Network.Affjax.DSL
  ( AffjaxRequest()
  , AffjaxRequestF(..)
  , affjaxRequest
  , url
  , method
  , header
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
import Network.Affjax.Requestable
import Network.HTTP.Method (Method())
import Network.HTTP.RequestHeader (RequestHeader())

-- | A free monad for building AJAX requests
type AffjaxRequest = FreeC AffjaxRequestF

-- | The request DSL AST.
data AffjaxRequestF a
  = SetURL String a
  | SetMethod Method a
  | AddHeader RequestHeader a
  | SetContent (Maybe AjaxContent) a
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

-- | Sets the URL for a request.
url :: String -> AffjaxRequest Unit
url url = liftFC (SetURL url unit)

-- | Sets the request method based on an HTTP verb.
method :: Method -> AffjaxRequest Unit
method meth = liftFC (SetMethod meth unit)

-- | Adds a header to the request.
header :: RequestHeader -> AffjaxRequest Unit
header header = liftFC (AddHeader header unit)

-- | Sets the content for the request.
content :: forall c. (AjaxRequestable c) => c -> AffjaxRequest Unit
content value = content' (Just value)

-- | Sets the content for the request, with the option of setting it to
-- | `Nothing`.
content' :: forall c. (AjaxRequestable c) => Maybe c -> AffjaxRequest Unit
content' value = liftFC (SetContent (toContent <$> value) unit)

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
