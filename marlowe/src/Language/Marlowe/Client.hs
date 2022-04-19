{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Marlowe.Client where
import Cardano.Api (AddressInEra (..), PaymentCredential (..), SerialiseAsRawBytes (serialiseToRawBytes), ShelleyEra,
                    StakeAddressReference (..))
import Cardano.Api.Shelley (StakeCredential (..))
import qualified Cardano.Api.Shelley as Shelley
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Monad.Error.Lens (catching, handling, throwing, throwing_)
import Control.Monad.Extra (concatMapM)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Marlowe.Client.History (History (..), MarloweTxOutRef, RolePayout (..), marloweHistory,
                                        marloweHistoryFrom, marloweUtxoStatesAt, toMarloweState, toRolePayout,
                                        txRoleData)
import Language.Marlowe.Scripts
import Language.Marlowe.Semantics
import qualified Language.Marlowe.Semantics as Marlowe
import Language.Marlowe.SemanticsTypes hiding (Contract, getAction)
import qualified Language.Marlowe.SemanticsTypes as Marlowe
import Language.Marlowe.Util (extractNonMerkleizedContractRoles)
import Ledger (CurrencySymbol, Datum (..), POSIXTime (..), PaymentPubKeyHash (..), PubKeyHash (..), TokenName,
               TxOut (..), TxOutRef, dataHash, txOutValue)
import qualified Ledger
import Ledger.Ada (adaSymbol, adaToken, adaValueOf, lovelaceValueOf)
import Ledger.Address (Address, StakePubKeyHash (StakePubKeyHash), pubKeyHashAddress, scriptHashAddress)
import Ledger.Constraints
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Interval as Interval
import Ledger.Scripts (datumHash, unitRedeemer)
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange, scSlotLength, slotToPOSIXTimeRange)
import qualified Ledger.Tx as Tx
import Ledger.Typed.Scripts
import qualified Ledger.Typed.Scripts as Typed
import qualified Ledger.Typed.Tx as Typed
import qualified Ledger.Value as Val
import Plutus.ChainIndex (ChainIndexTx (..), _ValidTx, citxOutputs)
import Plutus.Contract as Contract hiding (OtherContractError, _OtherContractError)
import qualified Plutus.Contract as Contract (ContractError (..))
import Plutus.Contract.Unsafe (unsafeGetSlotConfig)
import Plutus.Contract.Wallet (getUnspentOutput)
import qualified Plutus.Contracts.Currency as Currency
import Plutus.V1.Ledger.Api (toBuiltin)
import PlutusPrelude (foldMapM, (<|>))
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude as P



data MarloweClientInput = ClientInput InputContent
                        | ClientMerkleizedInput InputContent Marlowe.Contract
  deriving stock (Eq, Show, Generic)

instance FromJSON MarloweClientInput where
  parseJSON json = uncurry ClientMerkleizedInput <$> parseJSON json <|> ClientInput <$> parseJSON json

instance ToJSON MarloweClientInput where
  toJSON (ClientInput content)                    = toJSON content
  toJSON (ClientMerkleizedInput content contract) = toJSON (content, contract)


type CreateEndpointSchema = (UUID, AssocMap.Map Val.TokenName (AddressInEra ShelleyEra), Marlowe.Contract)
type ApplyInputsEndpointSchema = (UUID, MarloweParams, Maybe TimeInterval, [MarloweClientInput])
type ApplyInputsNonMerkleizedEndpointSchema = (UUID, MarloweParams, Maybe TimeInterval, [InputContent])
type AutoEndpointSchema = (UUID, MarloweParams, Party, POSIXTime)
type RedeemEndpointSchema = (UUID, MarloweParams, TokenName, AddressInEra ShelleyEra)
type CloseEndpointSchema = UUID

type MarloweSchema =
        Endpoint "create" CreateEndpointSchema
        .\/ Endpoint "apply-inputs" ApplyInputsEndpointSchema
        .\/ Endpoint "apply-inputs-nonmerkleized" ApplyInputsNonMerkleizedEndpointSchema
        .\/ Endpoint "auto" AutoEndpointSchema
        .\/ Endpoint "redeem" RedeemEndpointSchema
        .\/ Endpoint "close" CloseEndpointSchema

data MarloweEndpointResult =
    CreateResponse MarloweParams
    | ApplyInputsResponse
    | AutoResponse
    | RedeemResponse
    | CloseResponse
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

type MarloweCompanionSchema = EmptySchema
type MarloweFollowSchema = Endpoint "follow" MarloweParams


data MarloweError =
      TransitionError
    | AmbiguousOnChainState
    | UnableToExtractTransition
    | OnChainStateNotFound
    | MarloweEvaluationError TransactionError
    | OtherContractError ContractError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


makeClassyPrisms ''MarloweError

instance AsContractError MarloweError where
    _ContractError = _OtherContractError

instance AsCheckpointError MarloweError where
    _CheckpointError = _OtherContractError . _CheckpointError

data PartyAction
             = PayDeposit AccountId Party Token Integer
             | WaitForTimeout POSIXTime
             | WaitOtherActionUntil POSIXTime
             | NotSure
             | CloseContract
  deriving (Show)

type RoleOwners = AssocMap.Map Val.TokenName (AddressInEra ShelleyEra)

-- This data type contains all the information needed to reconstruct the
-- state of a Marlowe Contract.
data ContractHistory =
    ContractHistory
        { chParams         :: MarloweParams      -- ^ The "instance id" of the contract
        , chInitialData    :: MarloweData        -- ^ The initial Contract + State
        , chHistory        :: [TransactionInput] -- ^ All the transaction that affected the contract.
                                                 --   The current state and intermediate states can
                                                 --   be recalculated by using computeTransaction
                                                 --   of each TransactionInput to the initial state
        , chAddress        :: Address            -- ^ The script address of the marlowe contract
        , chUnspentPayouts :: UnspentPayouts     -- ^ All UTxOs associated with our payout script.
                                                 --   Please note that in theory we include here outpus
                                                 --   which possible were created by an "external" transactions.
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)

-- We need a semigroup instance to be able to update the state of the FollowerContract via `tell`.
-- For most of the fields we just use the initial values as they are not expected to change,
-- and we only concatenate new TransactionInputs
instance Semigroup ContractHistory where
    first <> second  =
        ContractHistory
            { chParams = chParams first
            , chInitialData = chInitialData first
            , chHistory = chHistory first <> chHistory second
            , chAddress = chAddress first
            , chUnspentPayouts = chUnspentPayouts second
            }

-- The FollowerContractState is a Maybe because the Contract monad requires the state
-- to have a Monoid instance. `Nothing` is the initial state of the contract, and then
-- with the first `tell` we have a valid initial ContractHistory
type FollowerContractState = Maybe ContractHistory

newtype OnChainState = OnChainState {ocsTxOutRef :: MarloweTxOutRef}

newtype Transition = Transition History     -- ^ The state machine instance transitioned to a new state
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PayoutsChange
              = PayoutSpent ChainIndexTx
              | PayoutProduced (NonEmpty ChainIndexTx)
              deriving stock (Show, Generic)


-- We expose unordered payouts set so we can compute this state
-- from the chain easily and we can avoid traversing the full history.
newtype UnspentPayouts = UnspentPayouts [RolePayout]
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
    deriving newtype (Semigroup, Monoid)


-- | The outcome of 'waitForUpdateTimeout'
created :: MarloweParams -> MarloweData -> FollowerContractState
created marloweParams marloweData = Just $ ContractHistory
              { chParams = marloweParams
              , chInitialData = marloweData
              , chHistory = []
              , chAddress = Typed.validatorAddress $ mkMarloweTypedValidator marloweParams
              , chUnspentPayouts = mempty
              }

transition :: MarloweParams -> TransactionInput -> UnspentPayouts -> FollowerContractState
transition marloweParams input unspentPayouts =
    let
        {-- NOTE: dummyMarloweData
          WARNING: The FollowerContractState needs to be a monoid so we can have a mempty
                   value when the contract is created, and then a Semigroup to be able to
                   add data to the contract state. The semigroup instance has the semantics
                   of choosing the first Params, Data and Address and to concatenate history.
                   We don't always have MarloweData in the context of a transition (for example
                   with Close), so we always use a dummyMarloweData that "should" be discarded.
                   The intended flow is that the State is initially Nothing, then the first real
                   value is forged with `created` and then we add more transactions to history
                   with this function. We wouldn't need this hack if we were able to query the
                   contract state from the Contract monad.
        --}
        dummyMarloweData =
            MarloweData
                { marloweContract = Close
                , marloweState = emptyState 0
                }
    in
        Just $ ContractHistory
              { chParams = marloweParams
              , chInitialData = dummyMarloweData
              , chHistory = [input]
              , chAddress = Typed.validatorAddress $ mkMarloweTypedValidator marloweParams
              , chUnspentPayouts = unspentPayouts
              }

data ContractProgress = InProgress | Finished
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup ContractProgress where
    _ <> Finished     = Finished
    any <> InProgress = any

instance Monoid ContractProgress where
    mempty = InProgress

type EndpointName = String

data EndpointResponse a err =
    EndpointSuccess UUID a
    -- TODO: The EndpointName should be a part of `err` if
    --       the user decides to, but we need to refactor MarloweError and
    --       the Marlowe Plutus App, so I leave this for a separate PR.
    | EndpointException UUID EndpointName err
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

-- The Semigroup instance is thought so that when we call `tell`, we inform
-- the FrontEnd of the last response. It is the responsability of the FE to
-- tie together a request and a response with the UUID.
instance Semigroup (EndpointResponse a err) where
    _ <> last      = last

type MarloweEndpointResponse = EndpointResponse MarloweEndpointResult MarloweError

type MarloweContractState = Maybe MarloweEndpointResponse

mkMarloweTypedValidator :: MarloweParams -> SmallTypedValidator
mkMarloweTypedValidator = smallUntypedValidator


minLovelaceDeposit :: Integer
minLovelaceDeposit = 2_000_000

{- NOTE: Chain index / cardano-node query consistency
  It seems that we are able to experience inconsistency between chain-index state and
  `cardano-node` state. For example we can be notified about `utxoIsProduced` but subsequent
  query about the same address can result in an empty set. To work around that problem we
  should probably provide some helper for check looping but for now we use trivial strategy
  of using ad hoc `waitNMilliSeconds` in multiple places.

  I think that consistency of a `Contract` block is just broken:

    * We have no way to express "transactionallity" - that we want to work against single the same tip
    in a particular query block.

    * We have no quarantees about consistency between async and sync parts of the Contract interpretion
    and the tip.


-}

waitForChainIndex :: forall st sc err. AsContractError err => Contract st sc err ()
waitForChainIndex = void $ waitNMilliSeconds 2_500

debugMsg :: String -> String -> String
debugMsg fnName msg = "[DEBUG:" <> fnName <> "] " <> msg

-- TODO: Move to debug log.
debug :: forall st sc err. String -> String -> Contract st sc err ()
debug fnName msg = logInfo $ debugMsg fnName msg

marloweFollowContract :: Contract FollowerContractState MarloweFollowSchema MarloweError ()
marloweFollowContract = awaitPromise $ endpoint @"follow" $ \params ->
  do
    debug' $ "call parameters: " <> show params <> "."
    let typedValidator = mkMarloweTypedValidator params

    unspentPayouts <- unspentPayoutsAtCurrency $ rolesCurrency params
    marloweHistory params
      >>= maybe (pure InProgress) (updateHistory params)
      >> (updatePayouts unspentPayouts params $> InProgress)
      >>= checkpointLoop (follow typedValidator params)

  where
    debug' = debug "Language.Marlowe.Client.marloweFollowContract"
    follow typedValidator params = \case
        Finished -> do
            debug "follow" $ "MarloweFollower found finished contract with " <> show params <> "."
            pure $ Right InProgress -- do not close the contract so we can see it in Marlowe Run history
        InProgress -> do
            waitForTransitionPromise <- waitForTransition typedValidator
            waitForPayoutPromise <- waitForPayoutsChange $ rolesCurrency params

            update <- awaitPromise (selectEither waitForPayoutPromise waitForTransitionPromise)
            -- FIXME: Super ugly workaround - see NOTE: Chain index / cardano-node query consistency
            waitForChainIndex

            -- Stateless payouts handling is just simpler - we unconditionally override
            -- the payouts information.
            unspentPayouts@(UnspentPayouts ps) <- unspentPayoutsAtCurrency $ rolesCurrency params
            debug' $ "| payouts | = "  <> show (length ps)

            -- FIXME: There is a possible subtle BUG here:
            --    * suppose that a payout and a transistion accure at in the same transaction
            --    * suppose that react to the payout instead of transition (we shouldn't because
            --        the above `selectEither` prioritize transition but who knows...)
            --    * then the transition is not handled because we start monitoring transitions
            --      from the next utxo onwards
            --    * the whole input history can be broken from that point onwards
            --
            -- We should probably move away from the statefull handling of the history and just
            -- recompute and override it in a similar fashion as we handle payouts.
            case update of
              Right t -> do
                case t of
                  Transition Closed{..} -> do
                      debug' $ "found contract closed with " <> show historyInput <> " by TxId " <> show historyTxId <> "."
                      tell @FollowerContractState (transition params historyInput unspentPayouts)
                      pure (Right Finished)
                  Transition InputApplied{..} -> do
                      debug' $ "found contract transitioned with " <> show historyInput <> " by " <> show historyTxOutRef <> "."
                      tell @FollowerContractState (transition params historyInput unspentPayouts)
                      pure (Right InProgress)
                  Transition Created{..} -> do
                      debug' $ "found contract created with " <> show historyData <> " by " <> show historyTxOutRef <> "."
                      tell @FollowerContractState (created params historyData)
                      pure (Right InProgress)
              Left payoutsChange -> do
                updatePayouts unspentPayouts params
                case payoutsChange of
                  PayoutSpent _ -> do
                    debug' "payout has just been spent"
                    pure (Right InProgress)
                  PayoutProduced _ -> do
                    debug' "payout has just been produced"
                    pure (Right InProgress)

    updatePayouts unspentPayouts@(UnspentPayouts ps) params = do
      debug "Language.Marlowe.Client.marloweFollowContract:updatePayouts" $ "| unspentPayouts | = " <> (show $ length ps)
      tell @FollowerContractState $ Just $ ContractHistory
        { chParams = params
        -- Please check NOTE: dummyMarloweData for more info
        , chInitialData = MarloweData
            { marloweContract = Close
            , marloweState = emptyState 0
            }
        , chHistory = []
        , chAddress = Typed.validatorAddress $ mkMarloweTypedValidator params
        , chUnspentPayouts = unspentPayouts
        }

    updateHistory :: MarloweParams
                  -> History
                  -> Contract FollowerContractState MarloweFollowSchema MarloweError ContractProgress
    updateHistory params Created{..} =
      do
        debug' $ "MarloweFollower found contract created with " <> show historyData <> " by " <> show historyTxOutRef <> "."
        tell $ created params historyData
        maybe (pure InProgress) (updateHistory params) historyNext
    updateHistory params InputApplied{..} =
      do
        debug' $ "MarloweFollower found contract transitioned with " <> show historyInput <> " by " <> show historyTxOutRef <> "."
        tell $ transition params historyInput mempty
        maybe (pure InProgress) (updateHistory params) historyNext
    updateHistory params Closed{..} =
      do
        debug' $ "MarloweFollower found contract closed with " <> show historyInput <> " by TxId " <> show historyTxId <> "."
        tell $ transition params historyInput mempty
        pure Finished

unspentPayoutsAtCurrency :: AsContractError e
                         => CurrencySymbol
                         -> Contract w s e UnspentPayouts
unspentPayoutsAtCurrency rolesCurrency = do
  let
    address = scriptHashAddress $ mkRolePayoutValidatorHash rolesCurrency
  txs <- do
    utxosMap <- utxosTxOutTxAt address
    pure $ fmap snd $ Map.elems utxosMap
  let
    rolePayoutTxs = concatMap txRoleData txs
  pure $ UnspentPayouts . map toRolePayout $ rolePayoutTxs


{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = selectList [create, apply, applyNonmerkleized, auto, redeem, close]
  where
    debug' endpoint msg = debug ("Language.Marlowe.Client.marlowePlutusContract:" <> endpoint) msg
    catchError reqId endpointName handler = catching _MarloweError
        (void $ mapError (review _MarloweError) handler)
        (\err -> do
            logWarn @String (show err)
            tell $ Just $ EndpointException reqId endpointName err
            marlowePlutusContract)
    -- [UC-CONTRACT-1][1] Start a new marlowe contract
    create = endpoint @"create" $ \(reqId, owners, contract) -> catchError reqId "create" $ do
        let
          debug'' = debug' "create"
        -- Create a transaction with the role tokens and pay them to the contract creator
        -- See Note [The contract is not ready]
        ownPubKey <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
        debug'' $ "ownPubKey = " <> show ownPubKey
        let roles = extractNonMerkleizedContractRoles contract
        debug'' $ "roles = " <> show roles
        (params, distributeRoleTokens, lkps) <- setupMarloweParams owners roles
        debug'' $ "params = " <> show params
        time <- currentTime
        debug'' $ "Marlowe contract created with parameters: " <> show params <> " at " <> show time
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = State
                    { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) minLovelaceDeposit
                    , choices  = AssocMap.empty
                    , boundValues = AssocMap.empty
                    , minTime = time } }
        debug'' $ "marloweData = " <> show marloweData
        let minAdaTxOut = lovelaceValueOf minLovelaceDeposit
        let typedValidator = mkMarloweTypedValidator params
        let tx = mustPayToTheScript marloweData minAdaTxOut <> distributeRoleTokens
        debug'' $ "tx = " <> show tx
        let lookups = Constraints.typedValidatorLookups typedValidator <> lkps
        debug'' $ "lookups = " <> show lookups
        -- Create the Marlowe contract and pay the role tokens to the owners
        utx <- either (throwing _ConstraintResolutionContractError) pure (Constraints.mkTx lookups tx)
        debug'' $ "utx = " <> show utx
        btx <- balanceTx $ Constraints.adjustUnbalancedTx utx
        debug'' $ "btx = " <> show btx
        stx <- submitBalancedTx btx
        debug'' $ "stx = " <> show stx
        let txId = Tx.getCardanoTxId stx
        awaitTxConfirmed txId
        debug'' $ "txId = " <> show txId
        debug'' $ "MarloweApp contract creation confirmed for parameters " <> show params <> "."
        tell $ Just $ EndpointSuccess reqId $ CreateResponse params
        marlowePlutusContract
    apply = endpoint @"apply-inputs" $ \(reqId, params, timeInterval, inputs) -> catchError reqId "apply-inputs" $ do
        let
          debug'' = debug' "apply-inputs"
        debug'' $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator timeInterval inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        debug'' $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
    applyNonmerkleized = endpoint @"apply-inputs-nonmerkleized" $ \(reqId, params, timeInterval, inputs) -> catchError reqId "apply-inputs-nonmerkleized" $ do
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator timeInterval $ ClientInput <$> inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        debug' "apply-inputs-nonmerkleized" $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
    redeem = promiseMap (mapError (review _MarloweError)) $ endpoint @"redeem" $ \(reqId, MarloweParams{rolesCurrency}, role, paymentAddress) -> catchError reqId "redeem" $ do
        let
          debug'' = debug' "redeem"
        debug'' $ "rolesCurrency = " <> show rolesCurrency
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        debug'' $ "address = " <> show address
        utxos <- utxosAt address
        let
          spendable txout =
            let
              expectedDatumHash = datumHash (Datum $ PlutusTx.toBuiltinData role)
              dh = either id Ledger.datumHash <$> preview Ledger.ciTxOutDatum txout
            in
              dh == Just expectedDatumHash
          utxosToSpend = Map.filter spendable utxos
          spendPayoutConstraints tx ref txout =
            do
              let amount = view Ledger.ciTxOutValue txout
              previousConstraints <- tx
              payOwner <- mustPayToShelleyAddress paymentAddress amount
              pure
                $ previousConstraints
                <> payOwner -- pay to a token owner
                <> Constraints.mustSpendScriptOutput ref unitRedeemer -- spend the rolePayoutScript address

        spendPayouts <- Map.foldlWithKey spendPayoutConstraints (pure mempty) utxosToSpend
        if spendPayouts == mempty
        then do
            debug'' $ "MarloweApp contract redemption empty for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse
        else do
            let
              constraints = spendPayouts
                  -- must spend a role token for authorization
                  <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
              -- lookup for payout validator and role payouts
              validator = rolePayoutScript rolesCurrency
            debug'' $ "constraints = " <> show constraints
            ownAddressLookups <- ownShelleyAddress paymentAddress
            let
              lookups = Constraints.otherScript validator
                  <> Constraints.unspentOutputs utxosToSpend
                  <> ownAddressLookups
            debug'' $ "lookups = " <> show lookups
            tx <- either (throwing _ConstraintResolutionContractError) pure (Constraints.mkTx @Void lookups constraints)
            debug'' $ "tx = " <> show tx
            _ <- submitTxConfirmed $ Constraints.adjustUnbalancedTx tx
            debug'' $ "MarloweApp contract redemption confirmed for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse

        marlowePlutusContract
    auto = endpoint @"auto" $ \(reqId, params, party, untilTime) -> catchError reqId "auto" $ do
        let typedValidator = mkMarloweTypedValidator params
        let continueWith :: MarloweData -> Contract MarloweContractState MarloweSchema MarloweError ()
            continueWith md@MarloweData{marloweContract} =
                if canAutoExecuteContractForParty party marloweContract
                then autoExecuteContract reqId params typedValidator party md
                else do
                    tell $ Just $ EndpointSuccess reqId AutoResponse
                    marlowePlutusContract

        maybeState <- getOnChainState typedValidator
        case maybeState of
            Nothing -> do
                waitForTimeoutOrTransition typedValidator untilTime >>= \case
                    Left _ -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Right (Transition Closed{}) -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Right (Transition InputApplied{historyData}) -> continueWith historyData
                    Right (Transition Created{historyData}) -> continueWith historyData
            Just OnChainState{ocsTxOutRef=st} -> do
                let marloweData = toMarloweState st
                continueWith marloweData
    -- The MarloweApp contract is closed implicitly by not returning
    -- itself (marlowePlutusContract) as a continuation
    close = endpoint @"close" $ \reqId -> tell $ Just $ EndpointSuccess reqId CloseResponse


    autoExecuteContract :: UUID
                      -> MarloweParams
                      -> SmallTypedValidator
                      -> Party
                      -> MarloweData
                      -> Contract MarloweContractState MarloweSchema MarloweError ()
    autoExecuteContract reqId params typedValidator party marloweData = do
        time <- currentTime
        let timeRange = (time, time + defaultTxValidationRange)
        let action = getAction timeRange party marloweData
        case action of
            PayDeposit acc p token amount -> do
                logInfo @String $ "PayDeposit " <> show amount <> " at within time " <> show timeRange
                let payDeposit = do
                        marloweData <- mkStep params typedValidator timeRange [ClientInput $ IDeposit acc p token amount]
                        continueWith marloweData
                catching _MarloweError payDeposit $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 seconds"
                    _ <- awaitTime (time + 2_000)
                    continueWith marloweData
            WaitForTimeout timeout -> do
                logInfo @String $ "WaitForTimeout " <> show timeout
                _ <- awaitTime timeout
                continueWith marloweData
            WaitOtherActionUntil timeout -> do
                logInfo @String $ "WaitOtherActionUntil " <> show timeout
                waitForTimeoutOrTransition typedValidator timeout >>= \case
                    Left _ -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith marloweData
                    Right (Transition Closed{}) -> do
                        logInfo @String $ "Contract Ended"
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Right (Transition InputApplied{historyData}) -> continueWith historyData
                    Right (Transition Created{historyData}) -> continueWith historyData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                let closeContract = do
                        _ <- mkStep params typedValidator timeRange []
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract

                catching _MarloweError closeContract $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry CloseContract in 2 seconds"
                    _ <- awaitTime (time + 2000)
                    continueWith marloweData
            NotSure -> do
                logInfo @String $ "NotSure"
                tell $ Just $ EndpointSuccess reqId AutoResponse
                marlowePlutusContract

          where
            continueWith = autoExecuteContract reqId params typedValidator party


setupMarloweParams
    :: forall s e i o a.
    (AsMarloweError e)
    => RoleOwners
    -> Set Val.TokenName
    -> Contract MarloweContractState s e
        (MarloweParams, TxConstraints i o, ScriptLookups a)
setupMarloweParams owners roles = mapError (review _MarloweError) $ do
    if Set.null roles
    then do
        let params = marloweParams adaSymbol
        pure (params, mempty, mempty)
    else if roles `Set.isSubsetOf` Set.fromList (AssocMap.keys owners)
    then do
        let tokens = (, 1) <$> Set.toList roles
        txOutRef@(Ledger.TxOutRef h i) <- getUnspentOutput
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:setupMarloweParams] txOutRef = " <> show txOutRef
        txOut <-
          maybe
            (throwing _ContractError . Contract.OtherContractError . T.pack $ show txOutRef <> " was not found on the chain index. Please verify that plutus-chain-index is 100% synced.")
            pure
            =<< txOutFromRef txOutRef
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:setupMarloweParams] txOut = " <> show txOut
        let utxo = Map.singleton txOutRef txOut
        let theCurrency = Currency.OneShotCurrency
                { curRefTransactionOutput = (h, i)
                , curAmounts              = AssocMap.fromList tokens
                }
            curVali     = Currency.curPolicy theCurrency
            lookups     = Constraints.mintingPolicy curVali
                            <> Constraints.unspentOutputs utxo
            mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                            <> Constraints.mustMintValue (Currency.mintedValue theCurrency)
        let rolesSymbol = Ledger.scriptCurrencySymbol curVali
        let minAdaTxOut = adaValueOf 2
        let giveToParty (role, addr) =
              mustPayToShelleyAddress addr (Val.singleton rolesSymbol role 1 <> minAdaTxOut)
        distributeRoleTokens <- foldMapM giveToParty $ AssocMap.toList owners
        let params = marloweParams rolesSymbol
        pure (params, mintTx <> distributeRoleTokens, lookups)
    else do
        let missingRoles = roles `Set.difference` Set.fromList (AssocMap.keys owners)
        let message = T.pack $ "You didn't specify owners of these roles: " <> show missingRoles
        throwing _ContractError $ Contract.OtherContractError message

ownShelleyAddress
  :: AddressInEra ShelleyEra
  -> Contract MarloweContractState s MarloweError (ScriptLookups Void)
ownShelleyAddress addr = Constraints.ownPaymentPubKeyHash . fst <$> shelleyAddressToKeys addr

mustPayToShelleyAddress
  :: AddressInEra ShelleyEra
  -> Val.Value
  -> Contract MarloweContractState s MarloweError (TxConstraints i o)
mustPayToShelleyAddress addr value = do
  (ppkh, skh) <- shelleyAddressToKeys addr
  pure $ ($ value) $ maybe
    (Constraints.mustPayToPubKey ppkh)
    (Constraints.mustPayToPubKeyAddress ppkh)
    skh

shelleyAddressToKeys
  :: AddressInEra ShelleyEra
  -> Contract MarloweContractState s MarloweError (PaymentPubKeyHash, Maybe StakePubKeyHash)
shelleyAddressToKeys (AddressInEra _ (Shelley.ShelleyAddress _ paymentCredential stakeRef)) =
  case Shelley.fromShelleyPaymentCredential paymentCredential of
    PaymentCredentialByScript _ -> throwError $ OtherContractError $ Contract.OtherContractError "Script payment addresses not supported"
    PaymentCredentialByKey hash ->
      let ppkh = PaymentPubKeyHash . PubKeyHash . toBuiltin $ serialiseToRawBytes hash
      in
        case Shelley.fromShelleyStakeReference stakeRef of
          StakeAddressByValue (StakeCredentialByScript _) ->
            throwError $ OtherContractError $ Contract.OtherContractError "Script stake addresses not supported"
          StakeAddressByPointer _ ->
            throwError $ OtherContractError $ Contract.OtherContractError "Pointer stake addresses not supported"
          NoStakeAddress -> pure (ppkh, Nothing)
          StakeAddressByValue (StakeCredentialByKey stakeHash) ->
            pure (ppkh,  Just . StakePubKeyHash . PubKeyHash . toBuiltin $ serialiseToRawBytes stakeHash)
shelleyAddressToKeys _ = throwError $ OtherContractError $ Contract.OtherContractError "Byron Addresses not supported"

getAction :: MarloweTimeRange -> Party -> MarloweData -> PartyAction
getAction timeRange party MarloweData{marloweContract,marloweState} = let
    env = Environment timeRange
    in case reduceContractUntilQuiescent env marloweState marloweContract of
        ContractQuiescent _reduced _warnings _payments state contract ->
            -- here the contract is either When or Close
            case contract of
                When [Case (Deposit acc depositParty tok value) _] _ _
                    | party == depositParty -> let
                        amount = Marlowe.evalValue env state value
                        in PayDeposit acc party tok amount
                When [Case (Deposit _ depositParty _ _) _] timeout _
                    | party /= depositParty    ->
                        WaitOtherActionUntil timeout
                When [] timeout _ -> WaitForTimeout timeout
                Close -> CloseContract
                _ -> NotSure
        -- When timeout is in the time range
        RRAmbiguousTimeIntervalError ->
            {- FIXME
                Consider contract:
                    When [cases] (POSIXTime 100) (When [Case Deposit Close]] (POSIXTime 105) Close)

                For a time range (95, 105) we get RRAmbiguousTimeIntervalError
                because timeout 100 is inside the time range.
                Now, we wait for time 105, and we miss the Deposit.

                To avoid that we need to know what was the original timeout
                that caused RRAmbiguousTimeIntervalError (i.e. POSIXTime 100).
                Then we'd rather wait until time 100 instead and would make the Deposit.
                I propose to modify RRAmbiguousTimeIntervalError to include the expected timeout.
             -}
            WaitForTimeout (snd timeRange)



canAutoExecuteContractForParty :: Party -> Marlowe.Contract -> Bool
canAutoExecuteContractForParty party = check
  where
    check cont =
        case cont of
            Close                                    -> True
            When [] _ cont                           -> check cont
            When [Case Deposit{} cont] _ timeoutCont -> check cont && check timeoutCont
            When cases _ timeoutCont                 -> all checkCase cases && check timeoutCont
            Pay _ _ _ _ cont                         -> check cont
            If _ c1 c2                               -> check c1 && check c2
            Let _ _ cont                             -> check cont
            Assert _ cont                            -> check cont


    checkCase (Case (Choice (ChoiceId _ p) _) cont) | p /= party = check cont
    checkCase _                                     = False


applyInputs :: AsMarloweError e
    => MarloweParams
    -> SmallTypedValidator
    -> Maybe TimeInterval
    -> [MarloweClientInput]
    -> Contract MarloweContractState MarloweSchema e MarloweData
applyInputs params typedValidator timeInterval inputs = mapError (review _MarloweError) $ do
    -- TODO: Move to debug log.
    do
      nowSlot <- currentSlot
      logInfo $ "[DEBUG:applyInputs] current slot = " <> show nowSlot
      logInfo $ "[DEBUG:applyInputs] time range for slot = " <> show (slotToPOSIXTimeRange unsafeGetSlotConfig nowSlot)
      nowTime <- currentTime
      logInfo $ "[DEBUG:applyInputs] current time = " <> show nowTime
    logInfo $ "[DEBUG:applyInputs] inputs = " <> show inputs
    logInfo $ "[DEBUG:applyInputs] params = " <> show params
    logInfo $ "[DEBUG:applyInputs] timeInterval = " <> show timeInterval
    let resolution = scSlotLength unsafeGetSlotConfig
    let floor'   (POSIXTime i) = POSIXTime $ resolution * (i `div` resolution)
    let ceiling' (POSIXTime i) = POSIXTime $ resolution * ((i + resolution - 1) `div` resolution)
    timeRange <- case timeInterval of
            Just (l, h) -> pure (ceiling' l, floor' h)
            Nothing -> do
                time <- currentTime
                pure (ceiling' time, floor' $ time + defaultTxValidationRange)
    -- TODO: Move to debug log.
    logInfo $ "[DEBUG:applyInputs] timeRange = " <> show timeRange
    mkStep params typedValidator timeRange inputs

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rolesCurrency = MarloweParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency}


defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


newtype CompanionState = CompanionState (Map MarloweParams MarloweData)
  deriving (Eq, Show)
  deriving (Semigroup,Monoid) via (Map MarloweParams MarloweData)

instance ToJSON CompanionState where
    toJSON (CompanionState m) = toJSON $ Map.toList m

instance FromJSON CompanionState where
    parseJSON v = CompanionState . Map.fromList <$> parseJSON v

{-|
    [UC-CONTRACT-2][0] Receive a role token for a marlowe contract

    Contract that monitors a user wallet for receiving a Marlowe role token.
    When it sees that a Marlowe contract exists on chain with a role currency
    of a token the user owns it updates its @CompanionState@
    with contract's @MarloweParams@ and @MarloweData@
-}
marloweCompanionContract :: Contract CompanionState MarloweCompanionSchema MarloweError ()
marloweCompanionContract = checkExistingRoleTokens
  where
    checkExistingRoleTokens = do
        -- Get the existing unspend outputs of the wallet that activated the companion contract
        pkh <- Contract.ownPaymentPubKeyHash
        let ownAddress = pubKeyHashAddress pkh Nothing
        -- Filter those outputs for role tokens and notify the WebSocket subscribers
        -- NOTE: CombinedWSStreamToServer has an API to subscribe to WS notifications
        utxo <- utxosAt ownAddress
        let txOuts = Ledger.toTxOut <$> Map.elems utxo
        forM_ txOuts notifyOnNewContractRoles
        -- This contract will run in a loop forever (because we always return Right)
        -- checking for updates to the UTXO's for a given address.
        -- The contract could be stopped via /contract/<instance>/stop but we are
        -- currently not doing that.
        checkpointLoop (fmap Right <$> checkForUpdates) ownAddress
    checkForUpdates ownAddress = do
        txns <- NonEmpty.toList <$> awaitUtxoProduced ownAddress
        -- See NOTE: Chain index / cardano-node query consistency
        waitForChainIndex
        let txOuts = txns >>= view (citxOutputs . _ValidTx)
        forM_ txOuts notifyOnNewContractRoles
        pure ownAddress

notifyOnNewContractRoles :: TxOut
    -> Contract CompanionState MarloweCompanionSchema MarloweError ()
notifyOnNewContractRoles txout = do
    -- Filter the CurrencySymbol's of this transaction output that might be
    -- a role token symbol. Basically, any non-ADA symbols is a prospect to
    -- to be a role token, but it could also be an NFT for example.
    let curSymbols = filterRoles txout
    forM_ curSymbols $ \cs -> do
        -- Check if there is a Marlowe contract on chain that uses this currency
        contract <- findMarloweContractsOnChainByRoleCurrency cs
        case contract of
            Just (params, md) -> do
                logInfo $ "WalletCompanion found currency symbol " <> show cs <> " with on-chain state " <> show (params, md) <> "."
                tell $ CompanionState (Map.singleton params md)
            Nothing           -> do
            -- The result will be empty if:
            --   * Note [The contract is not ready]: When you create a Marlowe contract first we create
            --                                       the role tokens, pay them to the contract creator and
            --                                       then we create the Marlowe contract.
            --   * If the marlowe contract is closed.
                -- TODO: Change for debug
                logWarn $ "WalletCompanion found currency symbol " <> show cs <> " but no on-chain state."
                pure ()


filterRoles :: TxOut -> [CurrencySymbol]
filterRoles TxOut { txOutValue, txOutDatumHash = Nothing } =
    let curSymbols = filter (/= adaSymbol) $ AssocMap.keys $ Val.getValue txOutValue
    in  curSymbols
filterRoles _ = []


findMarloweContractsOnChainByRoleCurrency
    :: CurrencySymbol
    -> Contract CompanionState
                MarloweCompanionSchema
                MarloweError
                (Maybe (MarloweParams, MarloweData))
findMarloweContractsOnChainByRoleCurrency curSym = do
    let params = marloweParams curSym
    let typedValidator = mkMarloweTypedValidator params
    maybeState <- handling _AmbiguousOnChainState (const $ pure Nothing) $ getOnChainState typedValidator
    case maybeState of
        Just OnChainState{ocsTxOutRef} -> do
            let marloweData = toMarloweState ocsTxOutRef
            pure $ Just (params, marloweData)
        Nothing -> pure Nothing


getOnChainStateTxOuts ::
    SmallTypedValidator
    -> Contract w schema MarloweError (Maybe (OnChainState, Map Ledger.TxOutRef Tx.ChainIndexTxOut))
getOnChainStateTxOuts validator = do
    (outRefs, utxos) <- mapError (review _MarloweError) $ marloweUtxoStatesAt validator
    case outRefs of
        []       -> pure Nothing
        [outRef] -> pure $ Just (OnChainState outRef, utxos)
        _        -> throwing_ _AmbiguousOnChainState

{-| Get the current on-chain state of the state machine instance.
    Return Nothing if there is no state on chain.
    Throws an @SMContractError@ if the number of outputs at the machine address is greater than one.
-}
getOnChainState ::
    SmallTypedValidator
    -> Contract w schema MarloweError (Maybe OnChainState)
getOnChainState validator = fmap fst <$> getOnChainStateTxOuts validator

mkStep ::
    MarloweParams
    -> SmallTypedValidator
    -> TimeInterval
    -> [MarloweClientInput]
    -> Contract w MarloweSchema MarloweError MarloweData
mkStep MarloweParams{..} typedValidator timeInterval@(minTime, maxTime) clientInputs = do
    logInfo $ "[DEBUG:mkStep] clientInputs = " <> show clientInputs
    let
      times =
        Interval.Interval
          (Interval.LowerBound (Interval.Finite minTime) True )
          (Interval.UpperBound (Interval.Finite maxTime) False)
      range' =
        posixTimeRangeToContainedSlotRange
          unsafeGetSlotConfig
          times
    maybeState <- getOnChainStateTxOuts typedValidator
    case maybeState of
        Nothing -> throwError OnChainStateNotFound
        Just (OnChainState{ocsTxOutRef}, utxo) -> do
            let currentState = toMarloweState ocsTxOutRef
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] currentState = " <> show currentState
            let marloweTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef

            (allConstraints, marloweData) <- evaluateTxContstraints currentState times marloweTxOutRef
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] allConstraints = " <> show allConstraints
            logInfo $ "[DEBUG:mkStep] marloweData = " <> show marloweData

            pk <- Contract.ownPaymentPubKeyHash
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] pk = " <> show pk
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                    <> Constraints.unspentOutputs utxo
            let lookups:: ScriptLookups TypedMarloweValidator
                lookups = lookups1 { Constraints.slOwnPaymentPubKeyHash = Just pk }
            utx <- either (throwing _ConstraintResolutionContractError)
                        pure
                        (Constraints.mkTx lookups allConstraints)
            let utx' = utx
                        {
                          unBalancedTxTx = (unBalancedTxTx utx) {Tx.txValidRange = range'}
                        , unBalancedTxValidityTimeRange = times
                        }
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] utx' = " <> show utx'
            btx <- balanceTx $ Constraints.adjustUnbalancedTx utx'
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] btx = " <> show btx
            stx <- submitBalancedTx btx
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] stx = " <> show stx
            let txId = Tx.getCardanoTxId stx
            awaitTxConfirmed txId
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:mkStep] txId = " <> show txId
            pure marloweData
  where
    evaluateTxContstraints :: MarloweData
        -> Ledger.POSIXTimeRange
        -> Tx.TxOutRef
        -> Contract w MarloweSchema MarloweError (TxConstraints [MarloweTxInput] MarloweData, MarloweData)
    evaluateTxContstraints MarloweData{..} times marloweTxOutRef = do
        let (inputs, inputsConstraints) = foldMap clientInputToInputAndConstraints clientInputs
        let txInput = TransactionInput {
                txInterval = timeInterval,
                txInputs = inputs }

        case computeTransaction txInput marloweState marloweContract of
            TransactionOutput {txOutPayments, txOutState, txOutContract} -> do
                let marloweData = MarloweData {
                        marloweContract = txOutContract,
                        marloweState = txOutState }
                let allConstraints = let
                        ownInputsConstraints =
                            [ ScriptInputConstraint
                                { icRedeemer = marloweTxInputsFromInputs inputs
                                , icTxOutRef = marloweTxOutRef
                                }
                            ]
                        payoutsByParty = AssocMap.toList $ P.foldMap payoutByParty txOutPayments
                        constraints = inputsConstraints
                            <> payoutConstraints payoutsByParty
                            <> mustValidateIn times
                        txConstraints = constraints { txOwnInputs = ownInputsConstraints
                                                    , txOwnOutputs = []
                                                    }
                        in case txOutContract of
                            Close -> txConstraints
                            _ -> let
                                finalBalance = let
                                    contractBalance = totalBalance (accounts marloweState)
                                    totalIncome = P.foldMap (collectDeposits . getInputContent) inputs
                                    totalPayouts = P.foldMap snd payoutsByParty
                                    in contractBalance P.+ totalIncome P.- totalPayouts
                                in txConstraints { txOwnOutputs =
                                    [ ScriptOutputConstraint
                                        { ocDatum = marloweData
                                        , ocValue = finalBalance
                                        }
                                    ]
                                    }
                pure (allConstraints, marloweData)

            Error e -> throwError $ MarloweEvaluationError e

    clientInputToInputAndConstraints :: MarloweClientInput -> ([Input], TxConstraints Void Void)
    clientInputToInputAndConstraints = \case
        ClientInput input -> ([NormalInput input], inputContentConstraints input)
        ClientMerkleizedInput input continuation -> let
            builtin = PlutusTx.toBuiltinData continuation
            hash = dataHash builtin
            constraints = inputContentConstraints input <> mustIncludeDatum (Datum builtin)
            in ([MerkleizedInput input hash continuation], constraints)
      where
        inputContentConstraints :: InputContent ->  TxConstraints Void Void
        inputContentConstraints input =
            case input of
                IDeposit _ party _ _         -> partyWitnessConstraint party
                IChoice (ChoiceId _ party) _ -> partyWitnessConstraint party
                INotify                      -> P.mempty
          where
            partyWitnessConstraint (PK pk)     = mustBeSignedBy (PaymentPubKeyHash pk)
            partyWitnessConstraint (Role role) = mustSpendRoleToken role

            mustSpendRoleToken :: TokenName -> TxConstraints Void Void
            mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1


    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> TxConstraints i0 o0
    payoutConstraints payoutsByParty = foldMap payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey (PaymentPubKeyHash pk) value
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                in mustPayToOtherScript rolePayoutValidatorHash dataValue value

waitForTransition ::
    forall w schema.
       SmallTypedValidator
    -> Contract w schema MarloweError (Promise w schema MarloweError Transition)
waitForTransition typedValidator = do
    let
      addr = validatorAddress typedValidator
      debug' = debug "Language.Marlowe.Client.waitForTransition"
    debug' $ "Marlowe validator address which we query" <> show addr
    currentState <- getOnChainState typedValidator
    case currentState of
            Nothing -> do
                debug' "Current state on chain is empty so waiting..."
                -- There is no on-chain state, so we wait for an output to appear
                -- at the address. Any output that appears needs to be checked
                -- with scChooser'
                pure $ promiseBind (utxoIsProduced addr) $ \txns -> do
                    -- See NOTE: Chain index / cardano-node query consistency
                    waitForChainIndex
                    produced <- concatMapM (marloweHistoryFrom typedValidator) $ NonEmpty.toList txns
                    case produced of
                        -- empty list shouldn't be possible, because we're waiting for txns with OnChainState
                        [history] -> pure $ Transition history
                        _         -> throwing_ _AmbiguousOnChainState
            Just OnChainState{ocsTxOutRef=Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef}} -> do
                debug' $ "wait till utxo is spent = " <> show tyTxOutRefRef
                pure $ promiseBind (utxoIsSpent tyTxOutRefRef) $ \txn -> do
                    -- See NOTE: Chain index / cardano-node query consistency
                    waitForChainIndex
                    spent <- marloweHistoryFrom typedValidator txn
                    case spent of
                        [history] -> pure $ Transition history
                        _         -> throwing_ _UnableToExtractTransition

waitForTimeoutOrTransition ::
    forall w schema.
       SmallTypedValidator
    -> POSIXTime
    -> Contract w schema MarloweError (Either POSIXTime Transition)
waitForTimeoutOrTransition validator timeout = do
  waitForTransitionPromise <- waitForTransition validator
  awaitPromise $ selectEither (isTime timeout) waitForTransitionPromise

-- | Actually safe version of `selectList` which returns a `Promise`
raceList :: [Promise w s e a] -> Promise w s e a
raceList [] = never
raceList l  = foldr1 select . reverse $ l

waitForPayoutsChange
  :: forall w schema.
    Val.CurrencySymbol
  -> Contract w schema MarloweError (Promise w schema MarloweError PayoutsChange)
waitForPayoutsChange rolesCurrency = do
  let
    debug' = debug "Language.Marlowe.Client.waitForPayoutsChange"
    address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)

    fromEither (Left u)  = PayoutSpent u
    fromEither (Right u) = PayoutProduced u
  payouts <- Map.keys <$> utxosAt address
  debug' $ "wait till payout is produced on rolepayout script hash = " <> show address
  debug' $ "wait till one of payouts is spent: " <> show payouts
  debug' $ "| payouts | = " <> show (length payouts) <> "."
  pure $ case payouts of
    [] -> PayoutProduced <$> utxoIsProduced address
    _  -> fromEither <$> selectEither (raceList $ fmap utxoIsSpent payouts) (utxoIsProduced address)

getInput ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> ChainIndexTx
    -> Maybe i
getInput outRef tx = do
    (_validator, Ledger.Redeemer r, _) <- listToMaybe $ mapMaybe Tx.inScripts $ filter (\Tx.TxIn{Tx.txInRef} -> outRef == txInRef) $ Set.toList $ _citxInputs tx
    PlutusTx.fromBuiltinData r
