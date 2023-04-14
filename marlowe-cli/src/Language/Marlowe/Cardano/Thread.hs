{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Type safe list of transactions representing on chain Marlowe execution.
--
-----------------------------------------------------------------------------

module Language.Marlowe.Cardano.Thread
  where

import qualified Cardano.Api as C
import Cardano.Api.Byron (TxIn)
import qualified Data.Aeson as A
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as List
import qualified Language.Marlowe as M

data Running

data Finished

data MarloweThread txInfo lang era status where
  Created
    :: txInfo
    -> TxIn
    -> MarloweThread txInfo lang era Running
  InputsApplied
    :: txInfo
    -> C.TxIn
    -> List.NonEmpty M.Input
    -> MarloweThread txInfo lang era Running
    -> MarloweThread txInfo lang era Running
  Closed
    :: txInfo
    -> [M.Input]
    -> MarloweThread txInfo lang era Running
    -> MarloweThread txInfo lang era Finished

foldlMarloweThread :: (forall status'. b -> MarloweThread txInfo lang era status' -> b) -> b -> MarloweThread txInfo lang era status -> b
foldlMarloweThread step acc m@(Closed _ _ th) = foldlMarloweThread step (step acc m) th
foldlMarloweThread step acc m@(InputsApplied _ _ _ th) = foldlMarloweThread step (step acc m) th
foldlMarloweThread step acc m = step acc m

foldrMarloweThread :: (forall status'. MarloweThread txInfo lang era status' -> b -> b) -> b -> MarloweThread txInfo lang era status -> b
foldrMarloweThread step acc m@(Closed _ _ th) = step m (foldrMarloweThread step acc th)
foldrMarloweThread step acc m@(InputsApplied _ _ _ th) = step m (foldrMarloweThread step acc th)
foldrMarloweThread step acc m = step m acc

-- Useful for debugging
marloweThreadToJSON :: MarloweThread txInfo lang era status -> A.Value
marloweThreadToJSON = A.toJSON . foldrMarloweThread step []
  where
    step :: MarloweThread txInfo lang era status' -> [A.Value] -> [A.Value]
    step (Created _ txIn) acc = A.object [("txIn", A.toJSON txIn)] : acc
    step (InputsApplied _ txIn inputs _) acc = A.object [("txIn", A.toJSON txIn), ("inputs", A.toJSON inputs)] : acc
    step (Closed _ inputs _) acc = A.object [("inputs", A.toJSON inputs)] : acc

-- In the `marlowe-runtime` we use this txIn to identify the contract.
marloweThreadInitialTxIn :: MarloweThread txInfo lang era status -> TxIn
marloweThreadInitialTxIn (Closed _ _ th) = marloweThreadInitialTxIn th
marloweThreadInitialTxIn (InputsApplied _ _ _ th) = marloweThreadInitialTxIn th
marloweThreadInitialTxIn (Created _ txIn) = txIn

marloweThreadTxIn :: MarloweThread txInfo lang era status -> Maybe TxIn
marloweThreadTxIn (Created _ txIn) = Just txIn
marloweThreadTxIn (InputsApplied _ txIn _ _) = Just txIn
marloweThreadTxIn Closed {} = Nothing

marloweThreadInputs :: MarloweThread txInfo lang era status -> [[M.Input]]
marloweThreadInputs = foldlMarloweThread step []
  where
    step :: [[M.Input]] -> MarloweThread txInfo lang era status' -> [[M.Input]]
    step acc (InputsApplied _ _ inputs _) = List.toList inputs : acc
    step acc (Closed _ inputs _) = inputs : acc
    step acc _ = acc

-- | Hides the `status` type parameter.
data AnyMarloweThread txInfo lang era where
  AnyMarloweThread :: MarloweThread txInfo lang era status -> AnyMarloweThread txInfo lang era

-- | Safec onstructors for `AnyMarloweThread`.
anyMarloweThreadCreated :: txInfo -> TxIn -> AnyMarloweThread txInfo lang era
anyMarloweThreadCreated txInfo txIn = AnyMarloweThread (Created txInfo txIn)

anyMarloweThread :: txInfo
 -> Maybe C.TxIn
 -> [M.Input]
 -> AnyMarloweThread txInfo lang era
 -> Maybe (AnyMarloweThread txInfo lang era)
anyMarloweThread txInfo mTxIn inputs (AnyMarloweThread th) = case (mTxIn, inputs) of
  (Just txIn, input:inputs') -> case th of
    m@Created {} -> Just $ AnyMarloweThread $ InputsApplied txInfo txIn (input :| inputs') m
    m@InputsApplied {} -> Just $ AnyMarloweThread $ InputsApplied txInfo txIn (input :| inputs') m
    _ -> Nothing

  (Nothing, _) -> case th of
    m@Created {} -> Just $ AnyMarloweThread $ Closed txInfo inputs m
    m@InputsApplied {} -> Just $ AnyMarloweThread $ Closed txInfo inputs m
    _ -> Nothing

  -- We disallow empty intermediate input application in here.
  -- Is there a non closing contract reduction which can be
  -- triggered like that?
  (Just _, []) -> Nothing

overAnyMarloweThread :: forall a era lang txInfo
                      . (forall status'. MarloweThread txInfo lang era status' -> a)
 -> AnyMarloweThread txInfo lang era
 -> a
overAnyMarloweThread f (AnyMarloweThread th) = f th

isRunning :: AnyMarloweThread txInfo lang era -> Bool
isRunning = do
  let
    step :: forall i l e s. MarloweThread i l e s -> Bool -> Bool
    step Closed {} _ = False
    step _ r = r
  overAnyMarloweThread (foldrMarloweThread step True)
