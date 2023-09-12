{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

-----------------------------------------------------------------------------

-- | Type safe list of transactions representing on chain Marlowe execution.
module Language.Marlowe.Cardano.Thread where

import Cardano.Api qualified as C
import Cardano.Api.Byron (TxIn)
import Data.Aeson qualified as A
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List
import Language.Marlowe qualified as M
import PlutusLedgerApi.V2 qualified as P

data Running

data Finished

-- | TODO:
-- | We should actually store all the info
-- | about the contract source, state and the currency
-- | symbol.
-- | so we can validate the transitions and redemptions.
data MarloweThread txInfo status where
  Created
    :: txInfo
    -> TxIn
    -> MarloweThread txInfo Running
  InputsApplied
    :: txInfo
    -> C.TxIn
    -> List.NonEmpty M.Input
    -> MarloweThread txInfo Running
    -> MarloweThread txInfo Running
  Closed
    :: txInfo
    -> [M.Input]
    -> MarloweThread txInfo Running
    -> MarloweThread txInfo Finished
  Redemption
    :: txInfo
    -> P.TokenName
    -> MarloweThread txInfo status
    -> MarloweThread txInfo status

foldlMarloweThread :: (forall status'. b -> MarloweThread txInfo status' -> b) -> b -> MarloweThread txInfo status -> b
foldlMarloweThread step acc m@(Closed _ _ th) = foldlMarloweThread step (step acc m) th
foldlMarloweThread step acc m@(InputsApplied _ _ _ th) = foldlMarloweThread step (step acc m) th
foldlMarloweThread step acc m = step acc m

foldrMarloweThread :: (forall status'. MarloweThread txInfo status' -> b -> b) -> b -> MarloweThread txInfo status -> b
foldrMarloweThread step acc m@(Closed _ _ th) = step m (foldrMarloweThread step acc th)
foldrMarloweThread step acc m@(InputsApplied _ _ _ th) = step m (foldrMarloweThread step acc th)
foldrMarloweThread step acc m = step m acc

-- Useful for debugging
marloweThreadToJSON :: MarloweThread txInfo status -> A.Value
marloweThreadToJSON = A.toJSON . foldrMarloweThread step []
  where
    step :: MarloweThread txInfo status' -> [A.Value] -> [A.Value]
    step (Created _ txIn) acc = A.object [("txIn", A.toJSON txIn)] : acc
    step (InputsApplied _ txIn inputs _) acc = A.object [("txIn", A.toJSON txIn), ("inputs", A.toJSON inputs)] : acc
    step (Redemption _ tokenName _) acc = A.object [("tokenName", A.toJSON $ show tokenName)] : acc
    step (Closed _ inputs _) acc = A.object [("inputs", A.toJSON inputs)] : acc

-- In the `marlowe-runtime` we use this txIn to identify the contract.
marloweThreadInitialTxIn :: MarloweThread txInfo status -> TxIn
marloweThreadInitialTxIn (Closed _ _ th) = marloweThreadInitialTxIn th
marloweThreadInitialTxIn (InputsApplied _ _ _ th) = marloweThreadInitialTxIn th
marloweThreadInitialTxIn (Redemption _ _ th) = marloweThreadInitialTxIn th
marloweThreadInitialTxIn (Created _ txIn) = txIn

marloweThreadTxIn :: MarloweThread txInfo status -> Maybe TxIn
marloweThreadTxIn (Created _ txIn) = Just txIn
marloweThreadTxIn (InputsApplied _ txIn _ _) = Just txIn
marloweThreadTxIn Closed{} = Nothing
marloweThreadTxIn (Redemption _ _ th) = marloweThreadTxIn th

marloweThreadInputs :: MarloweThread txInfo status -> [[M.Input]]
marloweThreadInputs = foldlMarloweThread step []
  where
    step :: [[M.Input]] -> MarloweThread txInfo status' -> [[M.Input]]
    step acc (InputsApplied _ _ inputs _) = List.toList inputs : acc
    step acc (Closed _ inputs _) = inputs : acc
    step acc Created{} = acc
    step acc Redemption{} = acc

marloweThreadTxInfos :: MarloweThread txInfo status -> [txInfo]
marloweThreadTxInfos = foldlMarloweThread step []
  where
    step :: [txInfo] -> MarloweThread txInfo status' -> [txInfo]
    step acc (Created txInfo _) = txInfo : acc
    step acc (InputsApplied txInfo _ _ _) = txInfo : acc
    step acc (Closed txInfo _ _) = txInfo : acc
    step acc (Redemption txInfo _ _) = txInfo : acc

-- | Hides the `status` type parameter.
data AnyMarloweThread txInfo where
  AnyMarloweThread :: MarloweThread txInfo status -> AnyMarloweThread txInfo

-- | Safe constructors for `AnyMarloweThread`.
anyMarloweThreadCreated :: txInfo -> TxIn -> AnyMarloweThread txInfo
anyMarloweThreadCreated txInfo txIn = AnyMarloweThread (Created txInfo txIn)

anyMarloweThreadRedeemed :: txInfo -> P.TokenName -> AnyMarloweThread txInfo -> AnyMarloweThread txInfo
anyMarloweThreadRedeemed txInfo tokenName (AnyMarloweThread th) = AnyMarloweThread $ Redemption txInfo tokenName th

anyMarloweThreadInputsApplied
  :: txInfo
  -> Maybe C.TxIn
  -> [M.Input]
  -> AnyMarloweThread txInfo
  -> Maybe (AnyMarloweThread txInfo)
anyMarloweThreadInputsApplied txInfo mTxIn inputs (AnyMarloweThread th) = do
  let skipRedemptions = \case
        Redemption _ _ sub -> skipRedemptions sub
        orig -> orig
      th' = skipRedemptions th
  case (mTxIn, inputs) of
    (Just txIn, input : inputs') -> case th' of
      Created{} -> Just $ AnyMarloweThread $ InputsApplied txInfo txIn (input :| inputs') th
      InputsApplied{} -> Just $ AnyMarloweThread $ InputsApplied txInfo txIn (input :| inputs') th
      _ -> Nothing
    (Nothing, _) -> case th' of
      Created{} -> Just $ AnyMarloweThread $ Closed txInfo inputs th
      InputsApplied{} -> Just $ AnyMarloweThread $ Closed txInfo inputs th
      _ -> Nothing
    -- We disallow empty intermediate input application in here.
    -- Is there a non closing contract reduction which can be
    -- triggered like that?
    (Just _, []) -> Nothing

overAnyMarloweThread
  :: forall a txInfo
   . (forall status'. MarloweThread txInfo status' -> a)
  -> AnyMarloweThread txInfo
  -> a
overAnyMarloweThread f (AnyMarloweThread th) = f th

isRunning :: AnyMarloweThread txInfo -> Bool
isRunning = do
  let step :: forall i s. MarloweThread i s -> Bool -> Bool
      step Closed{} _ = False
      step _ r = r
  overAnyMarloweThread (foldrMarloweThread step True)
