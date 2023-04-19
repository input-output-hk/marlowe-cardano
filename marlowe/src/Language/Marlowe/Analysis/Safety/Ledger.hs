-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Safety analysis of ledger rules for Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.Analysis.Safety.Ledger
  ( -- * Checks for Contracts
    checkMaximumValueBound
  , checkRoleNames
  , checkSafety
  , checkTokens
    -- * Worst-Case Bounds
  , worstDatumSize
  , worstMarloweData
  , worstMaximumValue
  , worstMinimumUtxo
  , worstRedeemerSize
  , worstState
  , worstTxOut
  , worstValue
  , worstValueSize
    -- * Enumeration
  , possibleRedeemers
    -- * Measurement
  , dataSize
  ) where


import Data.Foldable (maximumBy, toList)
import Data.Function (on)
import Data.List (nub, (\\))
import Language.Marlowe.Analysis.Safety.Types (SafetyError(..), SafetyReport(..))
import Language.Marlowe.Core.V1.Merkle (Continuations)
import Language.Marlowe.Core.V1.Plate (Extract, extractAllWithContinuations)
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Core.V1.Semantics.Types
  (Action(..), Bound(..), Case(..), Contract, InputContent(..), State(..), Token(..))
import Language.Marlowe.Scripts (MarloweTxInput(..))
import Numeric.Natural (Natural)
import Plutus.V2.Ledger.Api
  ( Credential(PubKeyCredential)
  , CurrencySymbol(..)
  , OutputDatum(OutputDatumHash)
  , StakingCredential(StakingHash)
  , ToData
  , TokenName(..)
  , TxOut(..)
  , adaSymbol
  , adaToken
  , toBuiltinData
  )
import PlutusTx.Builtins (serialiseData)

import qualified Data.Set as S (Set, filter, map, null, size)
import qualified Plutus.V1.Ledger.Value as V (singleton)
import qualified Plutus.V2.Ledger.Api as P (Address(..), Value)
import qualified PlutusTx.AssocMap as AM (Map, fromList, keys, toList)
import qualified PlutusTx.Prelude as P (lengthOfByteString)


-- | Check the safety of a Marlowe contract and state.
checkSafety
  :: Natural  -- ^ The `maxValueSize` protocol parameter.
  -> Integer  -- ^ The `utxoCostPerByte` protocol parameter.
  -> MarloweData  -- ^ The initial Marlowe data for the contract and state.
  -> Continuations  -- ^ The merkleized continuations of the contract.
  -> SafetyReport  -- ^ The report on the contract's safety.
checkSafety maxValueSize utxoCostPerByte MarloweData{..} continuations =
  let
    safetyErrors =
         checkRoleNames (rolesCurrency marloweParams /= adaSymbol) marloweContract continuations
      <> checkTokens marloweContract continuations
      <> checkMaximumValueBound maxValueSize marloweContract continuations
      <> checkPositiveBalance marloweState
      <> checkDuplicates marloweState
    boundOnMinimumUtxo = worstMinimumUtxo utxoCostPerByte marloweContract continuations
    boundOnDatumSize = worstDatumSize marloweParams marloweContract continuations
    boundOnRedeemerSize = worstRedeemerSize marloweContract continuations
  in
    SafetyReport{..}


checkPositiveBalance
  :: State
  -> [SafetyError]
checkPositiveBalance State{..} =
  fmap (uncurry NonPositiveBalance . fst)
    . filter ((<= 0) . snd)
    $ AM.toList accounts


checkDuplicates
  :: State
  -> [SafetyError]
checkDuplicates State{..} =
  let
    duplicates (AM.keys -> x) = x \\ nub x
  in
    fmap (uncurry DuplicateAccount) (duplicates accounts)
      <> fmap DuplicateChoice (duplicates choices)
      <> fmap DuplicateBoundValue (duplicates boundValues)


-- | Check that role names are not too long, and that roles are not present if a roles currency is not specified.
checkRoleNames
  :: Bool  -- ^ Whether the contract has a roles currency.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> [SafetyError]  -- ^ The safety messages.
checkRoleNames hasRolesCurrency contract continuations =
  let
    roles = extractAllWithContinuations contract continuations
    invalidRole TokenName{..} = P.lengthOfByteString unTokenName > 32
  in
    if hasRolesCurrency || S.null roles
      then fmap RoleNameTooLong . toList $ invalidRole `S.filter` roles
      else pure MissingRolesCurrency


-- | Check that a contract has native tokens satisfying the ledger rules.
checkTokens
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> [SafetyError]  -- ^ The safety messages.
checkTokens =
  let
    invalidToken token@(Token currency@CurrencySymbol{..} name@TokenName{..})
      | currency == adaSymbol = if name == adaToken then mempty else pure $ InvalidToken token
      | P.lengthOfByteString unCurrencySymbol /= 28 = pure $ InvalidCurrencySymbol currency
      | P.lengthOfByteString unTokenName > 32 = pure $ TokenNameTooLong name
      | otherwise = mempty
  in
   ((nub . foldMap invalidToken . toList) .) . extractAllWithContinuations


-- | Check that a contract satisfies the maximum value ledger constraint.
checkMaximumValueBound
  :: Natural  -- ^ The `maxValueSize` protocol parameter.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> [SafetyError]  -- ^ The safety messages.
checkMaximumValueBound maxValueSize contract continuations =
  let
    actual = worstMaximumValue contract continuations
  in
    if actual <= maxValueSize
      then mempty
      else pure $ MaximumValueMayExceedProtocol actual


-- | Compute a bound on the value size for a contract.
worstMaximumValue
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> Natural  -- ^ A bound on the value size (in bytes).
worstMaximumValue = (worstValueSize .) . extractAllWithContinuations


-- | Find a representative value with worst-case size.
worstValue
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> P.Value  -- ^ The value.
worstValue contract continuations =
  let
    tokens = extractAllWithContinuations contract continuations
    representativeValue (Token currency name) = V.singleton currency name big
  in
    V.singleton adaSymbol adaToken big
      <> foldMap representativeValue tokens


-- | Find a representative transaction output with worst-case size.
worstTxOut
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> TxOut  -- ^ The transaction output.
worstTxOut contract continuations =
  TxOut
  {
    txOutAddress =
      P.Address
      {
        addressCredential = PubKeyCredential "88888888888888888888888888888888888888888888888888888888"
      , addressStakingCredential = Just . StakingHash $ PubKeyCredential "99999999999999999999999999999999999999999999999999999999"
      }
  , txOutValue = worstValue contract continuations
  , txOutDatum = OutputDatumHash "5555555555555555555555555555555555555555555555555555555555555555"
  , txOutReferenceScript = Nothing
  }


-- | Compute a bound on the minimum UTxO that might be required for a Marlowe contract.
worstMinimumUtxo
  :: Integer  -- ^ The `utxoCostPerByte` protocol parameter.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> Integer  -- ^ A worst-case bound on the minimum UTxO lovelace for the contract.
worstMinimumUtxo utxoCostPerByte contract continuations =
  toInteger
    $ fromInteger utxoCostPerByte *
    (
        27 * 8                                    -- Worst case for stake address and ada.
      + worstMaximumValue contract continuations  -- Worst case for size of value.
      + 10 * 8                                    -- Worst case for length of datum hash.
    )  -- This assumes that the size computed for the Alonzo era serves as an upper-bound for future eras.


-- | Compute a bound on the size of a multi-asset value.
worstValueSize
  :: S.Set Token  -- ^ The tokens present.
  -> Natural      -- ^ The number of bytes on the ledger.
worstValueSize tokens =
  let
    -- Number of tokens.
    nTokens = toInteger $ S.size tokens
    -- Number of bytes needed to store the policy IDs.
    nPolicies = toInteger . S.size $ S.map (\(Token c _) -> c) tokens
    -- Number of bytes needed to store the token names.
    nNames = sum . fmap P.lengthOfByteString . toList $ S.map (\(Token _ (TokenName n)) -> n) tokens
    -- Round bytes up to whole words.
    padWords x = maximum [(x + 7) `div` 8, 1]
  in
    -- This is the ledger formula for computing the size of a token bundle.
    -- See <https://github.com/input-output-hk/cardano-ledger/blob/863f1d2f53852369802f070e16509ba3c896b47a/doc/explanations/min-utxo-alonzo.rst>.
    fromInteger $ 8 * (6 + padWords (13 * nTokens + 29 * nPolicies + nNames))


-- | Find a representative Marlowe state with worst-case size.
worstState
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> State  -- ^ A state with worst-case size.
worstState contract continuations =
  let
    worst :: (Extract k, Ord k) => AM.Map k Integer
    worst = AM.fromList . foldMap (pure . (, big)) $ extractAllWithContinuations contract continuations
    accounts = worst
    choices = worst
    boundValues = worst
    minTime = 9999999999999
  in
    State{..}


-- | Find a representative Marlowe datum with worst-case size.
worstMarloweData
  :: MarloweParams  -- ^ The Marlowe parameters.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> MarloweData  -- ^ A Marlowe datum of worst-case size.
worstMarloweData marloweParams marloweContract continuations =
  let
    marloweState = worstState marloweContract continuations
  in
    MarloweData{..}


-- | Compute a bound on the size of the Marlowe datum.
worstDatumSize
  :: MarloweParams  -- ^ The Marlowe parameters.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> Natural  -- ^ A worst-case bound on the size (in bytes) of the Marlowe datum.
worstDatumSize = ((dataSize .) .) . worstMarloweData


-- | Compute the representative redeemers for a Marlowe contract.
possibleRedeemers
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> [MarloweTxInput]  -- ^ The possible transaction inputs.
possibleRedeemers =
  let
    maximumAbs = maximumBy $ on compare abs
    largestBound (Bound x y) = maximumAbs [x, y]
    inputContent (Deposit a p t _) = IDeposit a p t big
    inputContent (Choice c bs) = IChoice c . maximumAbs $ largestBound <$> bs
    inputContent (Notify _) = INotify
    input :: Case Contract -> MarloweTxInput
    input (Case a _) = Input $ inputContent a
    input (MerkleizedCase a hash) = MerkleizedTxInput (inputContent a) hash
  in
    ((nub . fmap input . toList) .) . extractAllWithContinuations


-- | Compute a bound on the size of the Marlowe redeemer.
worstRedeemerSize
  :: Contract  -- ^ The contract.
  -> Continuations  -- ^ The merkleized continuations.
  -> Natural  -- ^ A worst-case bound on the size (in bytes) of the Marlowe redeemer.
worstRedeemerSize = ((maximum . fmap dataSize) .) . possibleRedeemers


-- | Measure the size of Plutus data.
dataSize :: ToData a => a -> Natural
dataSize = fromInteger . P.lengthOfByteString . serialiseData . toBuiltinData


-- | A big integer.
big :: Integral a => a
big = 2^(63::Int)
