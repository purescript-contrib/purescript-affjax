module Test.Main where

import Prelude
import Network.HTTP.Affjax as AX
import Effect.Aff (Aff, forkAff, attempt, runAff, killFiber)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (error, throwException)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Foreign (Foreign, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.StatusCode (StatusCode(..))

foreign import logAny :: forall a. a -> Effect Unit

logAny' :: forall a. a -> Aff Unit
logAny' = liftEffect <<< logAny

assertFail :: forall a. String -> Aff a
assertFail = throwError <<< error

assertMsg :: String -> Boolean -> Aff Unit
assertMsg _ true = pure unit
assertMsg msg false = assertFail msg

assertRight :: forall a b. Either a b -> Aff b
assertRight x = case x of
  Left y -> logAny' y >>= \_ -> assertFail "Expected a Right value"
  Right y -> pure y

assertLeft :: forall a b. Either a b -> Aff a
assertLeft x = case x of
  Right y -> logAny' y >>= \_ -> assertFail "Expected a Left value"
  Left y -> pure y

assertEq :: forall a. Eq a => Show a => a -> a -> Aff Unit
assertEq x y =
  when (x /= y) $ assertFail $ "Expected " <> show x <> ", got " <> show y

-- | For helping type inference
typeIs :: forall a. a -> Aff Unit
typeIs = const (pure unit)

main :: Effect Unit
main = void $ runAff (either (\e -> logShow e *> throwException e) (const $ log "affjax: All good!")) do
  let ok200 = StatusCode 200
  let notFound404 = StatusCode 404

  -- A complete URL is necessary for tests to work on Node.js
  let prefix = append "http://localhost:3838"
  let mirror       = prefix "/mirror"
  let doesNotExist = prefix "/does-not-exist"
  let notJson      = prefix "/not-json"
  let retryPolicy = AX.defaultRetryPolicy { timeout = Just (Milliseconds 500.0), shouldRetryWithStatusCode = \_ -> true }

  liftEffect $ log "GET /does-not-exist: should be 404 Not found after retries"
  (attempt $ AX.retry retryPolicy AX.affjax $ AX.defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse String)
    assertEq notFound404 res.status

  liftEffect $ log "GET /mirror: should be 200 OK"
  (attempt $ AX.affjax $ AX.defaultRequest { url = mirror }) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse Foreign)
    assertEq ok200 res.status

  liftEffect $ log "GET /does-not-exist: should be 404 Not found"
  (attempt $ AX.affjax $ AX.defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse String)
    assertEq notFound404 res.status

  liftEffect $ log "GET /not-json: invalid JSON with Foreign response should throw an error"
  void $ assertLeft =<< attempt (AX.get doesNotExist :: AX.Affjax Foreign)

  liftEffect $ log "GET /not-json: invalid JSON with String response should be ok"
  (attempt $ AX.get notJson) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse String)
    assertEq ok200 res.status

  liftEffect $ log "POST /mirror: should use the POST method"
  (attempt $ AX.post mirror "test") >>= assertRight >>= \res -> do
    assertEq ok200 res.status
    assertEq "POST" (_.method $ unsafeFromForeign res.response)

  liftEffect $ log "PUT with a request body"
  let content = "the quick brown fox jumps over the lazy dog"
  (attempt $ AX.put mirror content) >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse Foreign)
    assertEq ok200 res.status
    let mirroredReq = unsafeFromForeign res.response
    assertEq "PUT"   mirroredReq.method
    assertEq content mirroredReq.body

  liftEffect $ log "Testing CORS, HTTPS"
  (attempt $ AX.get "https://cors-test.appspot.com/test") >>= assertRight >>= \res -> do
    typeIs (res :: AX.AffjaxResponse Foreign)
    assertEq ok200 res.status
    -- assertEq (Just "test=test") (lookupHeader "Set-Cookie" res.headers)

  liftEffect $ log "Testing cancellation"
  forkAff (AX.post_ mirror "do it now") >>= killFiber (error "Pull the cord!")
  assertMsg "Should have been canceled" true
