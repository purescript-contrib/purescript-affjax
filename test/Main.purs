module Test.Main where

import Prelude

import Control.Monad.Aff
import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE(), log, print)
import qualified Control.Monad.Aff.Console as A
import Control.Monad.Eff.Exception
import Data.Either
import Data.Maybe
import Data.Foreign
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response
import Network.HTTP.Affjax.Request
import Network.HTTP.Method
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Network.HTTP.StatusCode

foreign import logAny
  :: forall e a. a -> Eff (console :: CONSOLE | e) Unit

logAny' :: forall e a. a -> Assert e Unit
logAny' = liftEff <<< logAny

type Assert e a = Aff (err :: EXCEPTION, console :: CONSOLE, ajax :: AJAX | e) a

assertFail :: forall e a. String -> Assert e a
assertFail msg = let e = error msg
                 in makeAff \errback _ -> errback e

assertMsg :: forall e. String -> Boolean -> Assert e Unit
assertMsg _   true  = return unit
assertMsg msg false = assertFail msg

assertRight :: forall e a b. Either a b -> Assert e b
assertRight x = case x of
  Left y -> logAny' y >>= \_ -> assertFail "Expected a Right value"
  Right y -> return y

assertLeft :: forall e a b. Either a b -> Assert e a
assertLeft x = case x of
  Right y -> logAny' y >>= \_ -> assertFail "Expected a Left value"
  Left y -> return y

assertEq :: forall e a. (Eq a, Show a) => a -> a -> Assert e Unit
assertEq x y = if x == y
                 then return unit
                 else assertFail $ "Expected " <> show x <> ", got " <> show y

-- | For helping type inference
typeIs :: forall e a. a -> Assert e Unit
typeIs = const (return unit)

main = runAff (\e -> print e >>= \_ -> throwException e) (const $ log "affjax: All good!") $ do
  let ok200 = StatusCode 200
  let notFound404 = StatusCode 404

  -- A complete URL is necessary for tests to work on Node.js
  let prefix = append "http://localhost:3838"
  let mirror       = prefix "/mirror"
  let doesNotExist = prefix "/does-not-exist"
  let notJson      = prefix "/not-json"
  let retryPolicy = defaultRetryPolicy { timeout = Just 500, shouldRetryWithStatusCode = \_ -> true }

  A.log "GET /does-not-exist: should be 404 Not found after retries"
  (attempt $ retry retryPolicy affjax $ defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
    typeIs (res :: AffjaxResponse String)
    assertEq notFound404 res.status

  A.log "GET /mirror: should be 200 OK"
  (attempt $ affjax $ defaultRequest { url = mirror }) >>= assertRight >>= \res -> do
    typeIs (res :: AffjaxResponse Foreign)
    assertEq ok200 res.status

  A.log "GET /does-not-exist: should be 404 Not found"
  (attempt $ affjax $ defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
    typeIs (res :: AffjaxResponse String)
    assertEq notFound404 res.status

  A.log "GET /not-json: invalid JSON with Foreign response should throw an error"
  assertLeft =<< attempt (get doesNotExist :: Affjax _ Foreign)

  A.log "GET /not-json: invalid JSON with String response should be ok"
  (attempt $ get notJson) >>= assertRight >>= \res -> do
    typeIs (res :: AffjaxResponse String)
    assertEq ok200 res.status

  A.log "POST /mirror: should use the POST method"
  (attempt $ post mirror "test") >>= assertRight >>= \res -> do
    assertEq ok200 res.status
    assertEq "POST" (_.method $ unsafeFromForeign res.response)

  A.log "PUT with a request body"
  let content = "the quick brown fox jumps over the lazy dog"
  (attempt $ put mirror content) >>= assertRight >>= \res -> do
    typeIs (res :: AffjaxResponse Foreign)
    assertEq ok200 res.status
    let mirroredReq = unsafeFromForeign res.response
    assertEq "PUT"   mirroredReq.method
    assertEq content mirroredReq.body

  A.log "Testing CORS, HTTPS"
  (attempt $ get "https://cors-test.appspot.com/test") >>= assertRight >>= \res -> do
    typeIs (res :: AffjaxResponse Foreign)
    assertEq ok200 res.status
    -- assertEq (Just "test=test") (lookupHeader "Set-Cookie" res.headers)

  A.log "Testing cancellation"
  canceler <- forkAff (post_ mirror "do it now")
  canceled <- canceler `cancel` error "Pull the cord!"
  assertMsg "Should have been canceled" canceled
