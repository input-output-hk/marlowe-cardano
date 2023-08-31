-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Safety analysis of ledger rules for Marlowe contracts.
module Language.Marlowe.Analysis.Safety.Ledger (
  -- * Checks for Contracts
  checkAddress,
  checkAddresses,
  checkContinuations,
  checkMaximumValueBound,
  checkNetwork,
  checkNetworks,
  checkRoleNames,
  checkSafety,
  checkTokens,

  -- * Worst-Case Bounds
  worstDatumSize,
  worstMarloweData,
  worstMaximumValue,
  worstMinimumUtxo,
  worstMinimumUtxo',
  worstRedeemerSize,
  worstState,
  worstTxOut,
  worstValue,
  worstValueSize,

  -- * Enumeration
  possibleRedeemers,

  -- * Measurement
  dataSize,
) where

import Data.Foldable (maximumBy, toList)
import Data.Function (on)
import Data.List (nub, (\\))
import Data.Maybe (fromJust)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..), SafetyReport (..))
import Language.Marlowe.Core.V1.Merkle (Continuations)
import Language.Marlowe.Core.V1.Plate (
  Extract,
  extractAddresses,
  extractAllWithContinuations,
  extractNetworks,
  extractRoleNames,
  extractTokens,
 )
import Language.Marlowe.Core.V1.Semantics (MarloweData (..), MarloweParams (..))
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (..),
  Bound (..),
  Case (..),
  Contract,
  InputContent (..),
  State (..),
  Token (..),
  emptyState,
 )
import Language.Marlowe.Core.V1.Semantics.Types.Address (
  Network,
  deserialiseAddressBech32,
  mainnet,
  serialiseAddressBech32,
  testnet,
 )
import Language.Marlowe.Scripts.Types (MarloweTxInput (..))
import Numeric.Natural (Natural)
import Plutus.V2.Ledger.Api (
  Credential (PubKeyCredential),
  CurrencySymbol (..),
  DatumHash (..),
  OutputDatum (OutputDatumHash),
  StakingCredential (StakingHash),
  ToData,
  TokenName (..),
  TxOut (..),
  adaSymbol,
  adaToken,
  toBuiltinData,
 )
import PlutusTx.Builtins (serialiseData)

import qualified Data.Map as M (keys)
import qualified Data.Set as S (Set, filter, foldr, map, null, size, toList)
import qualified Plutus.V1.Ledger.Value as V (singleton)
import qualified Plutus.V2.Ledger.Api as P (Address (..), Value)
import qualified PlutusTx.AssocMap as AM (Map, fromList, keys, toList)
import qualified PlutusTx.Prelude as P (lengthOfByteString)

-- | Check the safety of a Marlowe contract and state.
checkSafety
  :: Natural
  -- ^ The `maxValueSize` protocol parameter.
  -> Integer
  -- ^ The `utxoCostPerByte` protocol parameter.
  -> MarloweData
  -- ^ The initial Marlowe data for the contract and state.
  -> Continuations
  -- ^ The merkleized continuations of the contract.
  -> SafetyReport
  -- ^ The report on the contract's safety.
checkSafety maxValueSize utxoCostPerByte MarloweData{..} continuations =
  let state' = Just marloweState
      (networks, networkCheck) = checkNetworks state' marloweContract continuations
      safetyErrors =
        checkRoleNames (rolesCurrency marloweParams /= adaSymbol) state' marloweContract continuations
          <> checkTokens state' marloweContract continuations
          <> checkMaximumValueBound maxValueSize state' marloweContract continuations
          <> checkPositiveBalance marloweState
          <> checkDuplicates marloweState
          <> checkContinuations marloweContract continuations
          <> networkCheck
      boundOnMinimumUtxo = worstMinimumUtxo utxoCostPerByte marloweState marloweContract continuations
      boundOnDatumSize = worstDatumSize marloweParams marloweContract continuations
      boundOnRedeemerSize = worstRedeemerSize marloweContract continuations
   in SafetyReport{..}

-- | Check a contract for consistency with the network where it will be run.
checkNetwork
  :: Bool
  -- ^ Whether the network is mainnet.
  -> Maybe State
  -- ^ The contract's initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations of the contract.
  -> [SafetyError]
  -- ^ A safety error if the network is incorrect.
checkNetwork isMainnet state contract continuations =
  let networks = S.toList $ extractNetworks state contract continuations
      target
        | isMainnet = mainnet
        | otherwise = testnet
   in if all (== target) networks
        then mempty
        else pure WrongNetwork

-- | Check that networks are consistently used in a contract.
checkNetworks
  :: Maybe State
  -- ^ The contract's initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations of the contract.
  -> ([Network], [SafetyError])
  -- ^ The networks present in the contract, and any network errors.
checkNetworks state contract continuations =
  let networks = S.toList $ extractNetworks state contract continuations
   in ( networks
      , if length networks > 1
          then pure InconsistentNetworks
          else mempty
      )

-- | Check that an address can be serialized round trip.
checkAddress
  :: P.Address
  -- ^ The address.
  -> [SafetyError]
  -- ^ Any safety error for the address.
checkAddress address =
  case deserialiseAddressBech32 $ serialiseAddressBech32 False address of
    Nothing -> pure $ IllegalAddress address
    Just _ -> mempty

-- | Check that all addresses can be serialized round trip.
checkAddresses
  :: Maybe State
  -- ^ The contract's initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations of the contract.
  -> [SafetyError]
  -- ^ Any safety errors for the addresses in the contract.
checkAddresses state contract =
  nub
    . S.foldr ((<>) . checkAddress) mempty
    . extractAddresses state contract

-- | Check that all continuations are present.
checkContinuations
  :: Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations of the contract.
  -> [SafetyError]
  -- ^ Any reports of missing continuations.
checkContinuations contract continuations =
  let hashes = fmap DatumHash . S.toList $ extractAllWithContinuations contract continuations
   in MissingContinuation <$> filter (`notElem` M.keys continuations) hashes

-- | Check that all account balances are positive.
checkPositiveBalance
  :: State
  -- ^ The contract's state.
  -> [SafetyError]
  -- ^ Any reports of non-positive balances.
checkPositiveBalance State{..} =
  fmap (uncurry NonPositiveBalance . fst)
    . filter ((<= 0) . snd)
    $ AM.toList accounts

-- | Check that accounts, choices, or bound values in the contract's state are duplicated.
checkDuplicates
  :: State
  -- ^ The contract's state.
  -> [SafetyError]
  -- ^ Any reports of duplicates.
checkDuplicates State{..} =
  let duplicates (AM.keys -> x) = x \\ nub x
   in fmap (uncurry DuplicateAccount) (duplicates accounts)
        <> fmap DuplicateChoice (duplicates choices)
        <> fmap DuplicateBoundValue (duplicates boundValues)

-- | Check that role names are not too long, and that roles are not present if a roles currency is not specified.
checkRoleNames
  :: Bool
  -- ^ Whether the contract has a roles currency.
  -> Maybe State
  -- ^ The initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> [SafetyError]
  -- ^ The safety messages.
checkRoleNames hasRolesCurrency state contract continuations =
  let roles = extractRoleNames state contract continuations
      invalidRole TokenName{..} = P.lengthOfByteString unTokenName > 32
   in if hasRolesCurrency || S.null roles
        then fmap RoleNameTooLong . toList $ invalidRole `S.filter` roles
        else pure MissingRolesCurrency

-- | Check that a contract has native tokens satisfying the ledger rules.
checkTokens
  :: Maybe State
  -- ^ The initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> [SafetyError]
  -- ^ The safety messages.
checkTokens =
  let invalidToken token@(Token currency@CurrencySymbol{..} name@TokenName{..})
        | currency == adaSymbol = if name == adaToken then mempty else pure $ InvalidToken token
        | P.lengthOfByteString unCurrencySymbol /= 28 = pure $ InvalidCurrencySymbol currency
        | P.lengthOfByteString unTokenName > 32 = pure $ TokenNameTooLong name
        | otherwise = mempty
   in (((nub . foldMap invalidToken . toList) .) .) . extractTokens

-- | Check that a contract satisfies the maximum value ledger constraint.
checkMaximumValueBound
  :: Natural
  -- ^ The `maxValueSize` protocol parameter.
  -> Maybe State
  -- ^ The initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> [SafetyError]
  -- ^ The safety messages.
checkMaximumValueBound maxValueSize state contract continuations =
  let actual = worstMaximumValue state contract continuations
   in if actual <= maxValueSize
        then mempty
        else pure $ MaximumValueMayExceedProtocol actual

-- | Compute a bound on the value size for a contract.
worstMaximumValue
  :: Maybe State
  -- ^ The initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> Natural
  -- ^ A bound on the value size (in bytes).
worstMaximumValue =
  ((worstValueSize .) .) . extractTokens

-- | Find a representative value with worst-case size.
worstValue
  :: Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> P.Value
  -- ^ The value.
worstValue contract continuations =
  let tokens = extractAllWithContinuations contract continuations
      representativeValue (Token currency name) = V.singleton currency name big
   in V.singleton adaSymbol adaToken big
        <> foldMap representativeValue tokens

-- | Find a representative transaction output with worst-case size.
worstTxOut
  :: Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> TxOut
  -- ^ The transaction output.
worstTxOut contract continuations =
  TxOut
    { txOutAddress =
        P.Address
          { addressCredential = PubKeyCredential "88888888888888888888888888888888888888888888888888888888"
          , addressStakingCredential =
              Just . StakingHash $ PubKeyCredential "99999999999999999999999999999999999999999999999999999999"
          }
    , txOutValue = worstValue contract continuations
    , txOutDatum = OutputDatumHash "5555555555555555555555555555555555555555555555555555555555555555"
    , txOutReferenceScript = Nothing
    }

-- | Compute a bound on the minimum UTxO that might be required for a Marlowe contract, under the assumption that the contract does not pay funds from the accounts in the initial state.
worstMinimumUtxo
  :: Integer
  -- ^ The `utxoCostPerByte` protocol parameter.
  -> State
  -- ^ The initial state.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> Maybe Integer
  -- ^ A worst-case bound on the minimum UTxO lovelace for the contract, provided initial accounts are not payable in the contract.
worstMinimumUtxo utxoCostPerByte state@State{accounts} contract continuations =
  let accountTokens = extractAllWithContinuations contract continuations
   in if any (`elem` accountTokens) $ AM.keys accounts
        then Nothing
        else
          Just
            . toInteger
            $ fromInteger utxoCostPerByte
              * ( 27 * 8 -- Worst case for stake address and ada.
                    + worstMaximumValue (Just state) contract continuations -- Worst case for size of value.
                    + 10 * 8 -- Worst case for length of datum hash.
                ) -- This assumes that the size computed for the Alonzo era serves as an upper-bound for future eras.

-- | Compute a bound on the minimum UTxO that might be required for a Marlowe contract, under the assumption that the contract does not pay funds from the accounts in the initial state.
worstMinimumUtxo'
  :: Integer
  -- ^ The `utxoCostPerByte` protocol parameter.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> Integer
  -- ^ A worst-case bound on the minimum UTxO lovelace for the contract, provided initial accounts are not payable in the contract.
worstMinimumUtxo' utxoCostPerByte = (fromJust .) . worstMinimumUtxo utxoCostPerByte (emptyState 0)

-- | Compute a bound on the size of a multi-asset value.
worstValueSize
  :: S.Set Token
  -- ^ The tokens present.
  -> Natural
  -- ^ The number of bytes on the ledger.
worstValueSize tokens =
  let -- Number of tokens.
      nTokens = toInteger $ S.size tokens
      -- Number of bytes needed to store the policy IDs.
      nPolicies = toInteger . S.size $ S.map (\(Token c _) -> c) tokens
      -- Number of bytes needed to store the token names.
      nNames = sum . fmap P.lengthOfByteString . toList $ S.map (\(Token _ (TokenName n)) -> n) tokens
      -- Round bytes up to whole words.
      padWords x = maximum [(x + 7) `div` 8, 1]
   in -- This is the ledger formula for computing the size of a token bundle.
      -- See <https://github.com/input-output-hk/cardano-ledger/blob/863f1d2f53852369802f070e16509ba3c896b47a/doc/explanations/min-utxo-alonzo.rst>.
      fromInteger $ 8 * (6 + padWords (13 * nTokens + 29 * nPolicies + nNames))

-- | Find a representative Marlowe state with worst-case size.
worstState
  :: Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> State
  -- ^ A state with worst-case size.
worstState contract continuations =
  let worst :: (Extract k, Ord k) => AM.Map k Integer
      worst = AM.fromList . foldMap (pure . (,big)) $ extractAllWithContinuations contract continuations
      accounts = worst
      choices = worst
      boundValues = worst
      minTime = 9999999999999
   in State{..}

-- | Find a representative Marlowe datum with worst-case size.
worstMarloweData
  :: MarloweParams
  -- ^ The Marlowe parameters.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> MarloweData
  -- ^ A Marlowe datum of worst-case size.
worstMarloweData marloweParams marloweContract continuations =
  let marloweState = worstState marloweContract continuations
   in MarloweData{..}

-- | Compute a bound on the size of the Marlowe datum.
worstDatumSize
  :: MarloweParams
  -- ^ The Marlowe parameters.
  -> Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> Natural
  -- ^ A worst-case bound on the size (in bytes) of the Marlowe datum.
worstDatumSize = ((dataSize .) .) . worstMarloweData

-- | Compute the representative redeemers for a Marlowe contract.
possibleRedeemers
  :: Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> [MarloweTxInput]
  -- ^ The possible transaction inputs.
possibleRedeemers =
  let maximumAbs = maximumBy $ on compare abs
      largestBound (Bound x y) = maximumAbs [x, y]
      inputContent (Deposit a p t _) = IDeposit a p t big
      inputContent (Choice c bs) = IChoice c . maximumAbs $ largestBound <$> bs
      inputContent (Notify _) = INotify
      input :: Case Contract -> MarloweTxInput
      input (Case a _) = Input $ inputContent a
      input (MerkleizedCase a hash) = MerkleizedTxInput (inputContent a) hash
   in ((nub . fmap input . toList) .) . extractAllWithContinuations

-- | Compute a bound on the size of the Marlowe redeemer.
worstRedeemerSize
  :: Contract
  -- ^ The contract.
  -> Continuations
  -- ^ The merkleized continuations.
  -> Natural
  -- ^ A worst-case bound on the size (in bytes) of the Marlowe redeemer.
worstRedeemerSize = ((maximum . fmap dataSize) .) . possibleRedeemers

-- | Measure the size of Plutus data.
dataSize :: (ToData a) => a -> Natural
dataSize = fromInteger . P.lengthOfByteString . serialiseData . toBuiltinData

-- | A big integer.
big :: (Integral a) => a
big = 2 ^ (63 :: Int)
