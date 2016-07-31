module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, cancel, forkAff, attempt, runAff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Ref (REF)

import Data.Either (Either(..))
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Maybe (Maybe(..))

import Network.HTTP.Affjax as AX
import Network.HTTP.StatusCode (StatusCode(..))

foreign import logAny :: forall e a. a -> Eff (console :: CONSOLE | e) Unit

logAny' :: forall e a. a -> Assert e Unit
logAny' = liftEff <<< logAny

type Assert e a = Aff (err :: EXCEPTION, console :: CONSOLE, ajax :: AX.AJAX | e) a

assertFail :: forall e a. String -> Assert e a
assertFail msg = makeAff \errback _ -> errback (error msg)

assertMsg :: forall e. String -> Boolean -> Assert e Unit
assertMsg _ true = pure unit
assertMsg msg false = assertFail msg

assertRight :: forall e a b. Either a b -> Assert e b
assertRight x = case x of
  Left y -> logAny' y >>= \_ -> assertFail "Expected a Right value"
  Right y -> pure y

assertLeft :: forall e a b. Either a b -> Assert e a
assertLeft x = case x of
  Right y -> logAny' y >>= \_ -> assertFail "Expected a Left value"
  Left y -> pure y

assertEq :: forall e a. (Eq a, Show a) => a -> a -> Assert e Unit
assertEq x y =
  when (x /= y) $ assertFail $ "Expected " <> show x <> ", got " <> show y

-- | For helping type inference
typeIs :: forall e a. a -> Assert e Unit
typeIs = const (pure unit)

type MainEffects e =
  ( ref :: REF
  , avar :: AVAR
  , err :: EXCEPTION
  , console :: CONSOLE
  | e
  )

main :: Eff (MainEffects (ajax :: AX.AJAX)) Unit
main = void $ runAff (\e -> logShow e >>= \_ -> throwException e) (const $ log "affjax: All good!") $ do
  let ok200 = StatusCode 200
  let notFound404 = StatusCode 404

  -- A complete URL is necessary for tests to work on Node.js
  let prefix = append "http://localhost:3838"
  let mirror       = prefix "/mirror"
  let doesNotExist = prefix "/does-not-exist"
  let notJson      = prefix "/not-json"
  let retryPolicy = AX.defaultRetryPolicy { timeout = Just 500, shouldRetryWithStatusCode = \_ -> true }

  A.log "GET /does-not-exist: should be 404 Not found after retries"
  (attempt $ AX.retry retryPolicy AX.affjax $ AX.defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse String)
    assertEq notFound404 res.status

  A.log "GET /mirror: should be 200 OK"
  (attempt $ AX.affjax $ AX.defaultRequest { url = mirror }) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse Foreign)
    assertEq ok200 res.status

  A.log "GET /does-not-exist: should be 404 Not found"
  (attempt $ AX.affjax $ AX.defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse String)
    assertEq notFound404 res.status

  A.log "GET /not-json: invalid JSON with Foreign response should throw an error"
  assertLeft =<< attempt (AX.get doesNotExist :: AX.Affjax (MainEffects ()) Foreign)

  A.log "GET /not-json: invalid JSON with String response should be ok"
  (attempt $ AX.get notJson) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse String)
    assertEq ok200 res.status

  A.log "POST /mirror: should use the POST method"
  (attempt $ AX.post mirror "test") >>= assertRight >>= \res -> do
    assertEq ok200 res.status
    assertEq "POST" (_.method $ unsafeFromForeign res.response)

  A.log "PUT with a request body"
  let content = "the quick brown fox jumps over the lazy dog"
  (attempt $ AX.put mirror content) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse Foreign)
    assertEq ok200 res.status
    let mirroredReq = unsafeFromForeign res.response
    assertEq "PUT"   mirroredReq.method
    assertEq content mirroredReq.body

  A.log "Testing CORS, HTTPS"
  (attempt $ AX.get "https://cors-test.appspot.com/test") >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse Foreign)
    assertEq ok200 res.status
    -- assertEq (Just "test=test") (lookupHeader "Set-Cookie" res.headers)

  A.log "Testing cancellation"
  canceler <- forkAff (AX.post_ mirror "do it now")
  canceled <- canceler `cancel` error "Pull the cord!"
  assertMsg "Should have been canceled" canceled
