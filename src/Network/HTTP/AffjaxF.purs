module Network.HTTP.AffjaxF where

import Prelude

import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Eff.Exception (Error())

import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))

import Network.HTTP.Affjax (URL(), AffjaxRequest(), AffjaxResponse(), defaultRequest)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (Requestable)
import Network.HTTP.Affjax.Response (Respondable)

data AffjaxFP req res a =
  AffjaxFP
    (AffjaxRequest req)
    (Either Error (AffjaxResponse res) -> a)

type AffjaxF req res = AffjaxFP req res (Either Error (AffjaxResponse res))

instance functorAffjaxFP :: Functor (AffjaxFP req res) where
  map f (AffjaxFP req g) = AffjaxFP req (f <<< g)

-- | Makes an `Affjax` request.
affjax :: forall a b. (Requestable a, Respondable b) => AffjaxRequest a -> AffjaxF a b
affjax req = AffjaxFP req id

-- | Makes a `GET` request to the specified URL.
get :: forall a. (Respondable a) => URL -> AffjaxF Unit a
get u = affjax (defaultRequest { url = u, content = Nothing })

-- | Makes a `POST` request to the specified URL, sending data.
post :: forall a b. (Requestable a, Respondable b) => URL -> a -> AffjaxF a b
post u c = affjax (defaultRequest { method = Left POST, url = u, content = Just c })

-- | Makes a `POST` request to the specified URL with the option to send data.
post' :: forall a b. (Requestable a, Respondable b) => URL -> Maybe a -> AffjaxF a b
post' u c = affjax (defaultRequest { method = Left POST, url = u, content = c })

-- | Makes a `POST` request to the specified URL, sending data and ignoring the
-- | response.
post_ :: forall a. (Requestable a) => URL -> a -> AffjaxF a Unit
post_ = post

-- | Makes a `POST` request to the specified URL with the option to send data,
-- | and ignores the response.
post_' :: forall a. (Requestable a) => URL -> Maybe a -> AffjaxF a Unit
post_' = post'

-- | Makes a `PUT` request to the specified URL, sending data.
put :: forall a b. (Requestable a, Respondable b) => URL -> a -> AffjaxF a b
put u c = affjax (defaultRequest { method = Left PUT, url = u, content = Just c })

-- | Makes a `PUT` request to the specified URL with the option to send data.
put' :: forall a b. (Requestable a, Respondable b) => URL -> Maybe a -> AffjaxF a b
put' u c = affjax (defaultRequest { method = Left PUT, url = u, content = c })

-- | Makes a `PUT` request to the specified URL, sending data and ignoring the
-- | response.
put_ :: forall a. (Requestable a) => URL -> a -> AffjaxF a Unit
put_ = put

-- | Makes a `PUT` request to the specified URL with the option to send data,
-- | and ignores the response.
put_' :: forall a. (Requestable a) => URL -> Maybe a -> AffjaxF a Unit
put_' = put'

-- | Makes a `DELETE` request to the specified URL.
delete :: forall a. (Respondable a) => URL -> AffjaxF Unit a
delete u = affjax (defaultRequest { method = Left DELETE, url = u })

-- | Makes a `DELETE` request to the specified URL and ignores the response.
delete_ :: URL -> AffjaxF Unit Unit
delete_ = delete

eval
  :: forall req res eff a
   . (Requestable req, Respondable res)
  => AffjaxFP req res a
  -> Aff (ajax :: AX.AJAX | eff) a
eval (AffjaxFP req k) = k <$> attempt (AX.affjax req)
