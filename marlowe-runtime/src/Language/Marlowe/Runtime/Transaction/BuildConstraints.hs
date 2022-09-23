{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraints
  ( buildApplyInputsConstraints
  , buildCreateConstraints
  , buildWithdrawConstraints
  ) where

import Control.Category ((>>>))
import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Writer (execWriterT, tell)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Language.Marlowe.Runtime.ChainSync.Api
    ( Address,
      AssetId,
      SlotConfig,
      TokenName,
      Address,
      SlotConfig,
      TokenName,
      Address(Address),
      Assets(Assets),
      Lovelace(Lovelace),
      PaymentKeyHash(PaymentKeyHash),
      PolicyId(PolicyId),
      SlotConfig,
      SlotNo,
      TokenName(TokenName) )
import Language.Marlowe.Runtime.Core.Api
    ( Contract,
      MarloweVersion(MarloweV1),
      MarloweVersionTag(V1),
      PayoutDatum,
      Redeemer,
      TransactionScriptOutput,
      Contract,
      MarloweVersion,
      PayoutDatum,
      Redeemer,
      TransactionScriptOutput,
      Contract,
      MarloweVersion(MarloweV1),
      MarloweVersionTag(V1),
      PayoutDatum,
      Redeemer,
      TransactionScriptOutput(TransactionScriptOutput) )
import Language.Marlowe.Runtime.Transaction.Api
    ( ApplyInputsError,
      CreateError,
      WithdrawError,
      ApplyInputsError,
      CreateError,
      WithdrawError,
      ApplyInputsConstraintsBuildupError(MarloweComputeTransactionFailed),
      ApplyInputsError(ApplyInputsConstraintsBuildupFailed),
      CreateError,
      WithdrawError )
import Language.Marlowe.Runtime.Transaction.Constraints
    ( TxConstraints(..),
      TxConstraints,
      TxConstraints,
      mustConsumeMarloweOutput,
      mustPayToAddress,
      mustSendMarloweOutput,
      mustPayToRole,
      mustSpendRoleToken,
      requiresSignature )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Data.Traversable (for)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import qualified Plutus.V2.Ledger.Api as P
import qualified PlutusTx.AssocMap as AM

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraints
  :: MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> Map TokenName Address -- ^ The initial distribution of the role tokens.
  -> Map Int Aeson.Value -- ^ Extra metadata to add to the transaction.
  -> Contract v -- ^ The contract being instantiated.
  -> Either (CreateError v) (TxConstraints v)
buildCreateConstraints _ _ _ _ = error "not implemented"

-- | Creates a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraints
  :: SlotConfig -- ^ The slot config used to convert the validity interval to slots.
  -> MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> TransactionScriptOutput v -- ^ The previous script output for the contract
  -> Maybe UTCTime -- ^ The minimum bound of the validity interval (inclusive).
                   -- If not specified, the current time is used.
  -> Maybe UTCTime -- ^ The maximum bound of the validity interval (exclusive).
                   -- If not specified, this is computed from the the timeouts
                   -- in the contract.
  -> Redeemer v -- ^ The inputs to apply to the contract.
  -> Either (ApplyInputsError v) (TxConstraints v)
buildApplyInputsConstraints slotConfig version marloweOutput _ _ redeemer = do
  let
    invalidHereafter' = undefined
    invalidBefore' = undefined
  case version of
    MarloweV1 -> buildApplyInputsConstraintsV1 slotConfig marloweOutput invalidBefore' invalidHereafter' redeemer

-- | Creates a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraintsV1
  :: SlotConfig -- ^ The slot config used to convert the validity interval to slots.
  -> TransactionScriptOutput 'V1 -- ^ The previous script output for the contract with raw TxOut.
  -> UTCTime -- ^ The minimum bound of the validity interval (inclusive).
  -> UTCTime -- ^ The maximum bound of the validity interval (exclusive).
  -> Redeemer 'V1 -- ^ The inputs to apply to the contract.
  -> Either ApplyInputsError (TxConstraints 'V1)
-- buildApplyInputsConstraints _ _ _ _ _ = error "not implemented"
buildApplyInputsConstraintsV1 slotConfig marloweOutput invalidBefore invalidHereafter redeemer = execWriterT do
  let
    TransactionScriptOutput _ _ _ datum = marloweOutput
    V1.MarloweData params state contract = datum
    V1.MarloweParams currencySymbol = params

    invalidBefore' = slotStart slotConfig (utcToSlotNo slotConfig invalidBefore)
    invalidHereafter' = slotStart slotConfig (utcToSlotNo slotConfig invalidHereafter + 1)

    requiredParties = Set.fromList $ for redeemer $ marloweInputParty >>> maybeToList
    roleAssetId = toAssetId currencySymbol

  -- Construct inputs constraints.
  -- Consume UTXOs containing Marlowe script.
  tell $ mustConsumeMarloweOutput invalidBefore' invalidHereafter' redeemer

  -- Consume UTXOs containing all necessary role tokens and send them back.
  for_ requiredParties $ traverse_ $ \case
    V1.Role role -> tell $ mustSpendRoleToken $ roleAssetId role
    _ -> pure ()

  -- Require signature of an every party which is autorhized through an address.
  for_ requiredParties $ traverse_ $ \case
    V1.Address _ address -> case address of
      P.Address (P.PubKeyCredential (P.PubKeyHash pkh)) _ ->
        tell $ requiresSignature $ PaymentKeyHash $ P.fromBuiltin pkh
      _ -> pure ()
    _ -> pure ()

  -- Apply inputs.
  let
    txInterval = (invalidBefore', invalidHereafter')
    transactionInput = V1.TransactionInput { txInterval, txInputs = redeemer }
  (possibleContinuation, payments) <- case V1.computeTransaction transactionInput state contract of
     V1.Error err -> throwError $ ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed $ show err)
     V1.TransactionOutput _ payments _ V1.Close ->
       pure (Nothing, payments)
     V1.TransactionOutput _ payments state' contract' ->
       pure (Just (state', contract'), payments)

  -- Construct outputs constraints.
  -- Require Marlowe output if the contract is not closed.
  case possibleContinuation of
    Just (state'@V1.State { accounts }, contract') -> do
      let
        datum' = V1.MarloweData params state' contract'
        assets = moneyToAssets $ V1.totalBalance accounts
      tell $ mustSendMarloweOutput assets datum'
    Nothing -> pure ()

  -- For every payment require an output either to the role
  -- payout script or to directly to the party address.
  for payments \(V1.Payment _ payee token quantity) -> do
    let
      assets = case token of
        V1.Token "" "" -> Assets (Lovelace $ fromInteger quantity) mempty
        V1.Token cs tn -> do
          let
            assetId = toAssetId cs tn
            quantity' = CS.Quantity $ fromInteger quantity
          Assets (Lovelace 0) (CS.Tokens $ Map.singleton assetId quantity')
    case payee of
      V1.Party (V1.Address net addr) -> do
        let addr' = Address $ V1.serialiseAddress net addr
        tell $ mustPayToAddress assets addr'
      V1.Party (V1.Role role) ->
        tell $ mustPayToRole assets $ roleAssetId role
      V1.Account _ -> pure ()

  where
    utcToSlotNo :: SlotConfig -> UTCTime -> SlotNo
    utcToSlotNo = error "not implemented"

    slotStart :: SlotConfig -> SlotNo -> P.POSIXTime
    slotStart = error "not implemented"

    marloweInputContent (V1.NormalInput c) = c
    marloweInputContent (V1.MerkleizedInput c _ _) = c

    marloweInputParty = marloweInputContent >>> \case
      V1.IDeposit _ party _ _         -> Just party
      V1.IChoice (V1.ChoiceId _ party) _ -> Just party
      V1.INotify                      -> Nothing

    fromPlutusTokenName :: P.TokenName -> TokenName
    fromPlutusTokenName = TokenName . P.fromBuiltin . P.unTokenName

    fromPlutusCurrencySymbol :: P.CurrencySymbol -> PolicyId
    fromPlutusCurrencySymbol = PolicyId . P.fromBuiltin . P.unCurrencySymbol

    moneyToAssets :: V1.Money -> Assets
    moneyToAssets = Assets <$> moneyToLovelace <*> moneyToTokens

    moneyToLovelace :: V1.Money -> Lovelace
    moneyToLovelace = Lovelace . maybe 0 fromIntegral . (AM.lookup "" <=< AM.lookup "") . P.getValue

    toAssetId cs role = do
      let
        policyId = fromPlutusCurrencySymbol cs
        tokenName = fromPlutusTokenName role
      CS.AssetId policyId tokenName

    moneyToTokens :: V1.Money -> CS.Tokens
    moneyToTokens = CS.Tokens
      . Map.fromList
      . fmap
          ( bimap (uncurry toAssetId) fromIntegral
          . assocLeft
          )
      . (traverse AM.toList <=< AM.toList)
      . AM.delete ""
      . P.getValue

    assocLeft (a, (b, c)) = ((a, b), c)

-- | Creates a set of Tx constraints that are used to build a transaction that
-- withdraws payments from a payout validator.
buildWithdrawConstraints
  :: MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> PayoutDatum v -- ^ The role token from which to withdraw funds.
  -> Either (WithdrawError v) (TxConstraints v)
buildWithdrawConstraints = \case
  MarloweV1 -> Right . buildWithdrawConstraintsV1
  where
    buildWithdrawConstraintsV1 :: AssetId -> TxConstraints 'V1
    buildWithdrawConstraintsV1 =
      TxConstraints.mustConsumePayouts <> TxConstraints.mustSpendRoleToken
