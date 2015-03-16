module Network.Affjax.DSL
  ( AffjaxRequest()
  , AffjaxRequestF(..)
  , affjaxRequest
  , url
  , method
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
import Network.Affjax.HTTP
import Network.Affjax.Request

-- | A free monad for building AJAX requests
type AffjaxRequest c = FreeC (AffjaxRequestF c)

-- | The request DSL AST.
data AffjaxRequestF c a
  = SetURL String a
  | SetMethod Method a
  | AddHeader Header a
  | SetContent (Maybe (Content c)) a
  | SetUsername (Maybe String) a
  | SetPassword (Maybe String) a

-- | The interpreter for the request DSL AST.
affjaxN :: forall c. Natural (AffjaxRequestF c) (State (AjaxRequest c))
affjaxN (SetURL url a) = const a <$> modify (_ { url = url })
affjaxN (SetMethod method a) = const a <$> modify (_ { method = method })
affjaxN (AddHeader header a) = const a <$> modify (\req -> req { headers = header : req.headers })
affjaxN (SetContent content a) = const a <$> modify (_ { content = content })
affjaxN (SetUsername username a) = const a <$> modify (_ { username = username })
affjaxN (SetPassword password a) = const a <$> modify (_ { password = password })

-- | Runs the DSL, producing an `AjaxRequest` object.
affjaxRequest :: forall c a. AffjaxRequest c a -> AjaxRequest c
affjaxRequest = (`execState` defaultRequest) <<< runFreeCM affjaxN

-- | Sets the URL for a request.
url :: forall c. String -> AffjaxRequest c Unit
url url = liftFC (SetURL url unit)

-- | Sets the request method based on an HTTP verb.
method :: forall c. Method -> AffjaxRequest c Unit
method meth = liftFC (SetMethod meth unit)

-- | Adds a header to the request using a key and value.
header :: forall c. HeaderHead -> String -> AffjaxRequest c Unit
header key value = header' (Header key value)

-- | Adds a header to the request using a `Header` record.
header' :: forall c. Header -> AffjaxRequest c Unit
header' header = liftFC (AddHeader header unit)

-- | Sets the content for the request.
content :: forall c. Content c -> AffjaxRequest c Unit
content value = content' (Just value)

-- | Sets the content for the request, with the option of setting it to
-- | `Nothing`.
content' :: forall c. Maybe (Content c) -> AffjaxRequest c Unit
content' value = liftFC (SetContent value unit)

-- | Sets the username for the request.
username :: forall c. String -> AffjaxRequest c Unit
username value = username' (Just value)

-- | Sets the username for the request, with the option of setting it to
-- | `Nothing`.
username' :: forall c. Maybe String -> AffjaxRequest c Unit
username' value = liftFC (SetUsername value unit)

-- | Sets the password for the request.
password :: forall c. String -> AffjaxRequest c Unit
password value = password' (Just value)

-- | Sets the password for the request, with the option of setting it to
-- | `Nothing`.
password' :: forall c. Maybe String -> AffjaxRequest c Unit
password' value = liftFC (SetPassword value unit)
