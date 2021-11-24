module Cardano.Wasm where

import Prologue
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

-- import Data.ArrayBuffer.Types (Uint8Array)
foreign import data CardanoWasm :: Type

foreign import data Uint8Array :: Type

foreign import fromHexString_ :: EffectFn3 (forall x. x -> Maybe x) (forall x. Maybe x) String (Maybe Uint8Array)

fromHexString :: String -> Effect (Maybe Uint8Array)
fromHexString = runEffectFn3 fromHexString_ Just Nothing

foreign import loadCardanoWasm_ :: EffectFn1 Unit (Promise CardanoWasm)

loadCardanoWasm :: Aff CardanoWasm
loadCardanoWasm = toAffE $ runEffectFn1 loadCardanoWasm_ unit

foreign import data BigNum :: Type

foreign import bigNumFromStr_ :: EffectFn2 CardanoWasm String BigNum

bigNumFromStr :: CardanoWasm -> String -> Effect BigNum
bigNumFromStr = runEffectFn2 bigNumFromStr_

foreign import bigNumToStr_ :: EffectFn1 BigNum String

bigNumToStr :: BigNum -> Effect String
bigNumToStr = runEffectFn1 bigNumToStr_

foreign import freeObject_ :: forall a. EffectFn1 a Unit

-- TODO: add type constraint
freeObject :: forall a. a -> Effect Unit
freeObject = runEffectFn1 freeObject_

withBigNum :: forall b. String -> (BigNum -> Aff b) -> Aff b
withBigNum str f =
  bracket
    ( do
        wasm <- loadCardanoWasm
        liftEffect $ bigNumFromStr wasm str
    )
    (liftEffect <<< freeObject)
    f

-- TODO: revisar monad bracket
-- class FreableObject a
--   freeObject
foreign import data Value :: Type

foreign import newValue_ :: EffectFn2 CardanoWasm BigNum Value

newValue :: forall b. BigNum -> (Value -> Aff b) -> Aff b
newValue bigNum f =
  bracket
    ( do
        wasm <- loadCardanoWasm
        liftEffect $ runEffectFn2 newValue_ wasm bigNum
    )
    (liftEffect <<< freeObject)
    f

foreign import coin_ :: EffectFn1 Value BigNum

coin :: forall b. Value -> (BigNum -> Aff b) -> Aff b
coin val f =
  bracket
    ( liftEffect $ runEffectFn1 coin_ val
    )
    (liftEffect <<< freeObject)
    f

doStuff :: Aff String
doStuff =
  withBigNum "20"
    ( \bign ->
        newValue bign
          ( \val ->
              coin val
                ( \bign' ->
                    liftEffect $ bigNumToStr bign'
                )
          )
    )

doStuff' :: Aff String
doStuff' =
  let
    newValue' = flip newValue

    coin' = flip coin
  in
    (withBigNum "20")
      $ newValue'
          ( \val ->
              coin val
                ( \bign' ->
                    liftEffect $ bigNumToStr bign'
                )
          )

doStuff'' :: Aff String
doStuff'' =
  let
    newValue' = flip newValue

    coin' = flip coin
  in
    (withBigNum "20")
      $ newValue'
      $ coin'
          ( \bign' ->
              liftEffect $ bigNumToStr bign'
          )
