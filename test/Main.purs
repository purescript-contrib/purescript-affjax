module Test.Main where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core as J
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, finally, forkAff, killFiber, runAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as A
import Effect.Console (log, logShow)
import Effect.Exception (error, throwException)
import Foreign.Object as FO

foreign import logAny :: forall a. a -> Effect Unit

foreign import data Server :: Type
foreign import startServer :: EffectFnAff { server :: Server, port :: Int }
foreign import stopServer :: Server -> EffectFnAff Unit

logAny' :: forall a. a -> Aff Unit
logAny' = liftEffect <<< logAny

assertFail :: forall a. String -> Aff a
assertFail = throwError <<< error

assertMsg :: String -> Boolean -> Aff Unit
assertMsg _ true = pure unit
assertMsg msg false = assertFail msg

assertRight :: forall a b. Either a b -> Aff b
assertRight x = case x of
  Left y -> logAny' y *> assertFail "Expected a Right value"
  Right y -> pure y

assertLeft :: forall a b. Either a b -> Aff a
assertLeft x = case x of
  Right y -> logAny' y *> assertFail "Expected a Left value"
  Left y -> pure y

assertEq :: forall a. Eq a => Show a => a -> a -> Aff Unit
assertEq x y =
  when (x /= y) $ assertFail $ "Expected " <> show x <> ", got " <> show y

main :: Effect Unit
main = void $ runAff (either (\e -> logShow e *> throwException e) (const $ log "affjax: All good!")) do
  let ok200 = StatusCode 200
  let notFound404 = StatusCode 404

  { server, port } <- fromEffectFnAff startServer
  finally (fromEffectFnAff (stopServer server)) do
    A.log ("Test server running on port " <> show port)

    let prefix = append ("http://localhost:" <> show port)
    let mirror = prefix "/mirror"
    let doesNotExist = prefix "/does-not-exist"
    let notJson = prefix "/not-json"
    let slow = prefix "/slow"

    A.log "GET /mirror: should be 200 OK"
    (AX.request $ AX.defaultRequest { url = mirror }) >>= assertRight >>= \res -> do
      assertEq ok200 res.status

    A.log "GET /does-not-exist: should be 404 Not found"
    (AX.request $ AX.defaultRequest { url = doesNotExist }) >>= assertRight >>= \res -> do
      assertEq notFound404 res.status

    A.log "GET /not-json: invalid JSON with Foreign response should return an error"
    AX.get ResponseFormat.json doesNotExist >>= assertLeft >>= case _ of
      AX.ResponseBodyError _ _ -> pure unit
      other -> logAny' other *> assertFail "Expected a ResponseBodyError"

    A.log "GET /not-json: invalid JSON with String response should be ok"
    AX.get ResponseFormat.string notJson >>= assertRight >>= \res -> do
      assertEq ok200 res.status

    A.log "GET /slow with timeout: should return an error"
    (AX.request $ AX.defaultRequest { url = slow, timeout = Just (Milliseconds 100.0) }) >>= assertLeft >>= case _ of
      AX.TimeoutError -> pure unit
      other -> logAny' other *> assertFail "Expected a TimeoutError"

    A.log "POST /mirror: should use the POST method"
    AX.post ResponseFormat.json mirror (Just (RequestBody.string "test")) >>= assertRight >>= \res -> do
      assertEq ok200 res.status
      assertEq (Just "POST") (J.toString =<< FO.lookup "method" =<< J.toObject res.body)

    A.log "PUT with a request body"
    let content = "the quick brown fox jumps over the lazy dog"
    AX.put ResponseFormat.json mirror (Just (RequestBody.string content)) >>= assertRight >>= \res -> do
      assertEq ok200 res.status
      assertEq (Just "PUT") (J.toString =<< FO.lookup "method" =<< J.toObject res.body)
      assertEq (Just content) (J.toString =<< FO.lookup "body" =<< J.toObject res.body)

    A.log "Testing CORS, HTTPS"
    AX.get ResponseFormat.json "https://cors-test.appspot.com/test" >>= assertRight >>= \res -> do
      assertEq ok200 res.status

    A.log "Testing cancellation"
    forkAff (AX.post_ mirror (Just (RequestBody.string "do it now"))) >>= killFiber (error "Pull the cord!")
    assertMsg "Should have been canceled" true
