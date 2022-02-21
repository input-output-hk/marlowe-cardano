{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Client where
import Cardano.Api (AddressInEra (..), PaymentCredential (..), SerialiseAsRawBytes (serialiseToRawBytes), ShelleyEra,
                    StakeAddressReference (..))
import Cardano.Api.Shelley (StakeCredential (..))
import qualified Cardano.Api.Shelley as Shelley
import Control.Lens
import Control.Monad (forM_, void)
import Control.Monad.Error.Lens (catching, throwing, throwing_)
import Control.Monad.Extra (concatMapM)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing, listToMaybe, mapMaybe)
import Data.Monoid (First (..))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Language.Marlowe.Client.History (History (..), MarloweTxOutRef, marloweHistory, marloweHistoryFrom,
                                        marloweUtxoStatesAt, toMarloweState)
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
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Constraints.TxConstraints
import qualified Ledger.Interval as Interval
import Ledger.Scripts (datumHash, unitRedeemer)
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange)
import qualified Ledger.Tx as Tx
import Ledger.Typed.Scripts
import qualified Ledger.Typed.Scripts as Typed
import qualified Ledger.Typed.Tx as Typed
import qualified Ledger.Value as Val
import Plutus.ChainIndex (ChainIndexTx (..), _ValidTx, citxOutputs)
import Plutus.Contract as Contract
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


type MarloweSchema =
        Endpoint "create" (UUID, AssocMap.Map Val.TokenName (AddressInEra ShelleyEra), Marlowe.Contract)
        .\/ Endpoint "apply-inputs" (UUID, MarloweParams, Maybe SlotInterval, [MarloweClientInput])
        .\/ Endpoint "apply-inputs-nonmerkleized" (UUID, MarloweParams, Maybe SlotInterval, [InputContent])
        .\/ Endpoint "auto" (UUID, MarloweParams, Party, POSIXTime)
        .\/ Endpoint "redeem" (UUID, MarloweParams, TokenName, AddressInEra ShelleyEra)
        .\/ Endpoint "close" UUID

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

data ContractHistory =
    ContractHistory
        { chParams  :: First (MarloweParams, MarloweData)
        , chHistory :: [TransactionInput]
        , chAddress :: First Address
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ContractHistory)


newtype OnChainState = OnChainState {ocsTxOutRef :: MarloweTxOutRef}


-- | The outcome of 'waitForUpdateTimeout'
data WaitingResult t
    = Timeout t          -- ^ The timeout happened before any change of the on-chain state was detected
    | Transition History -- ^ The state machine instance transitioned to a new state
  deriving stock (Show,Generic,Functor)
  deriving anyclass (ToJSON, FromJSON)


created :: MarloweParams -> MarloweData -> ContractHistory
created p d = mempty
              {
                chParams = First (Just (p, d)),
                chAddress = First . Just . Typed.validatorAddress $ mkMarloweTypedValidator p
              }

transition :: TransactionInput -> ContractHistory
transition i = mempty{chHistory = [i] }

isEmpty :: ContractHistory -> Bool
isEmpty = isNothing . getFirst . chParams

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
minLovelaceDeposit = 2000000


marloweFollowContract :: Contract ContractHistory MarloweFollowSchema MarloweError ()
marloweFollowContract = awaitPromise $ endpoint @"follow" $ \params ->
  do
    let typedValidator = mkMarloweTypedValidator params
    marloweHistory params
      >>= maybe (pure InProgress) (updateHistory params)
      >>= checkpointLoop (follow typedValidator params)

  where
    follow typedValidator params = \case
        Finished -> do
            logInfo $ "MarloweFollower found finished contract with " <> show params <> "."
            pure $ Right InProgress -- do not close the contract so we can see it in Marlowe Run history
        InProgress -> do
            result <- waitForUpdateTimeout typedValidator never >>= awaitPromise
            case result of
                Timeout t -> absurd t
                Transition Closed{..} -> do
                    logInfo $ "MarloweFollower found contract closed with " <> show historyInput <> " by TxId " <> show historyTxId <> "."
                    tell @ContractHistory (transition historyInput)
                    pure (Right Finished)
                Transition InputApplied{..} -> do
                    logInfo $ "MarloweFollower found contract transitioned with " <> show historyInput <> " by " <> show historyTxOutRef <> "."
                    tell @ContractHistory (transition historyInput)
                    pure (Right InProgress)
                Transition Created{..} -> do
                    logInfo $ "MarloweFollower found contract created with " <> show historyData <> " by " <> show historyTxOutRef <> "."
                    tell @ContractHistory (created params historyData)
                    pure (Right InProgress)

    updateHistory :: MarloweParams
                  -> History
                  -> Contract ContractHistory MarloweFollowSchema MarloweError ContractProgress
    updateHistory params Created{..} =
      do
        logInfo $ "MarloweFollower found contract created with " <> show historyData <> " by " <> show historyTxOutRef <> "."
        tell $ created params historyData
        maybe (pure InProgress) (updateHistory params) historyNext
    updateHistory params InputApplied{..} =
      do
        logInfo $ "MarloweFollower found contract transitioned with " <> show historyInput <> " by " <> show historyTxOutRef <> "."
        tell $ transition historyInput
        maybe (pure InProgress) (updateHistory params) historyNext
    updateHistory _ Closed{..} =
      do
        logInfo $ "MarloweFollower found contract closed with " <> show historyInput <> " by TxId " <> show historyTxId <> "."
        tell $ transition historyInput
        pure Finished

{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = selectList [create, apply, applyNonmerkleized, auto, redeem, close]
  where
    catchError reqId endpointName handler = catching _MarloweError
        (void $ mapError (review _MarloweError) handler)
        (\err -> do
            logWarn @String (show err)
            tell $ Just $ EndpointException reqId endpointName err
            marlowePlutusContract)
    -- [UC-CONTRACT-1][1] Start a new marlowe contract
    create = endpoint @"create" $ \(reqId, owners, contract) -> catchError reqId "create" $ do
        -- Create a transaction with the role tokens and pay them to the contract creator
        -- See Note [The contract is not ready]
        ownPubKey <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
        let roles = extractNonMerkleizedContractRoles contract
        (params, distributeRoleTokens, lkps) <- setupMarloweParams owners roles
        slot <- currentTime
        logInfo $ "Marlowe contract created with parameters: " <> show params <> " at " <> show slot
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = State
                    { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) minLovelaceDeposit
                    , choices  = AssocMap.empty
                    , boundValues = AssocMap.empty
                    , minTime = slot } }
        let minAdaTxOut = lovelaceValueOf minLovelaceDeposit
        let typedValidator = mkMarloweTypedValidator params
        let tx = mustPayToTheScript marloweData minAdaTxOut <> distributeRoleTokens
        let lookups = Constraints.typedValidatorLookups typedValidator <> lkps
        -- Create the Marlowe contract and pay the role tokens to the owners
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
        submitTxConfirmed utx
        logInfo $ "MarloweApp contract creation confirmed for parameters " <> show params <> "."
        tell $ Just $ EndpointSuccess reqId $ CreateResponse params
        marlowePlutusContract
    apply = endpoint @"apply-inputs" $ \(reqId, params, slotInterval, inputs) -> catchError reqId "apply-inputs" $ do
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator slotInterval inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        logInfo $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
    applyNonmerkleized = endpoint @"apply-inputs-nonmerkleized" $ \(reqId, params, slotInterval, inputs) -> catchError reqId "apply-inputs-nonmerkleized" $ do
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator slotInterval $ ClientInput <$> inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        logInfo $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
    redeem = promiseMap (mapError (review _MarloweError)) $ endpoint @"redeem" $ \(reqId, MarloweParams{rolesCurrency}, role, paymentAddress) -> catchError reqId "redeem" $ do
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        utxos <- utxosAt address
        let
          spendPayoutConstraints tx ref txout = do
            let expectedDatumHash = datumHash (Datum $ PlutusTx.toBuiltinData role)
                amount = view Ledger.ciTxOutValue txout
                dh = either id Ledger.datumHash <$> preview Ledger.ciTxOutDatum txout
            case dh of
                Just datumHash | datumHash == expectedDatumHash ->
                    -- we spend the rolePayoutScript address
                    (Constraints.mustSpendScriptOutput ref unitRedeemer <>)
                    -- and pay to a token owner
                        <$> mustPayToShelleyAddress paymentAddress amount
                _ -> tx

        spendPayouts <- Map.foldlWithKey spendPayoutConstraints (pure mempty) utxos
        if spendPayouts == mempty
        then do
            logInfo $ "MarloweApp contract redemption empty for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse
        else do
            let
              constraints = spendPayouts
                  -- must spend a role token for authorization
                  <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
              -- lookup for payout validator and role payouts
              validator = rolePayoutScript rolesCurrency
            ownAddressLookups <- ownShelleyAddress paymentAddress
            let
              lookups = Constraints.otherScript validator
                  <> Constraints.unspentOutputs utxos
                  <> ownAddressLookups
            tx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx @Void lookups constraints)
            _ <- submitTxConfirmed $ Constraints.adjustUnbalancedTx tx
            logInfo $ "MarloweApp contract redemption confirmed for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse

        marlowePlutusContract
    auto = endpoint @"auto" $ \(reqId, params, party, untilSlot) -> catchError reqId "auto" $ do
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
                wr <- waitForUpdateUntilSlot typedValidator untilSlot
                case wr of
                    Transition Closed{} -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Transition InputApplied{historyData} -> continueWith historyData
                    Transition Created{historyData} -> continueWith historyData
            Just (OnChainState{ocsTxOutRef=st}, _) -> do
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
        slot <- currentTime
        let slotRange = (slot, slot + defaultTxValidationRange)
        let action = getAction slotRange party marloweData
        case action of
            PayDeposit acc p token amount -> do
                logInfo @String $ "PayDeposit " <> show amount <> " at within slots " <> show slotRange
                let payDeposit = do
                        marloweData <- mkStep params typedValidator slotRange [ClientInput $ IDeposit acc p token amount]
                        continueWith marloweData
                catching _MarloweError payDeposit $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 slots"
                    _ <- awaitTime (slot + 2)
                    continueWith marloweData
            WaitForTimeout timeout -> do
                logInfo @String $ "WaitForTimeout " <> show timeout
                _ <- awaitTime timeout
                continueWith marloweData
            WaitOtherActionUntil timeout -> do
                logInfo @String $ "WaitOtherActionUntil " <> show timeout
                wr <- waitForUpdateUntilSlot typedValidator timeout
                case wr of
                    Transition Closed{} -> do
                        logInfo @String $ "Contract Ended"
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith marloweData
                    Transition InputApplied{historyData} -> continueWith historyData
                    Transition Created{historyData} -> continueWith historyData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                let closeContract = do
                        _ <- mkStep params typedValidator slotRange []
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract

                catching _MarloweError closeContract $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry CloseContract in 2 slots"
                    _ <- awaitTime (slot + 2)
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
        txOut <- fromJust <$> txOutFromRef txOutRef  -- TODO: Review for safety.
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
        throwing _ContractError $ OtherError message

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
    PaymentCredentialByScript _ -> throwError $ OtherContractError $ OtherError "Script payment addresses not supported"
    PaymentCredentialByKey hash ->
      let ppkh = PaymentPubKeyHash . PubKeyHash . toBuiltin $ serialiseToRawBytes hash
      in
        case Shelley.fromShelleyStakeReference stakeRef of
          StakeAddressByValue (StakeCredentialByScript _) ->
            throwError $ OtherContractError $ OtherError "Script stake addresses not supported"
          StakeAddressByPointer _ ->
            throwError $ OtherContractError $ OtherError "Pointer stake addresses not supported"
          NoStakeAddress -> pure (ppkh, Nothing)
          StakeAddressByValue (StakeCredentialByKey stakeHash) ->
            pure (ppkh,  Just . StakePubKeyHash . PubKeyHash . toBuiltin $ serialiseToRawBytes stakeHash)
shelleyAddressToKeys _ = throwError $ OtherContractError $ OtherError "Byron Addresses not supported"

getAction :: MarloweSlotRange -> Party -> MarloweData -> PartyAction
getAction slotRange party MarloweData{marloweContract,marloweState} = let
    env = Environment slotRange
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
        -- When timeout is in the slot range
        RRAmbiguousSlotIntervalError ->
            {- FIXME
                Consider contract:
                    When [cases] (POSIXTime 100) (When [Case Deposit Close]] (POSIXTime 105) Close)

                For a slot range (95, 105) we get RRAmbiguousSlotIntervalError
                because timeout 100 is inside the slot range.
                Now, we wait for slot 105, and we miss the Deposit.

                To avoid that we need to know what was the original timeout
                that caused RRAmbiguousSlotIntervalError (i.e. POSIXTime 100).
                Then we'd rather wait until slot 100 instead and would make the Deposit.
                I propose to modify RRAmbiguousSlotIntervalError to include the expected timeout.
             -}
            WaitForTimeout (snd slotRange)



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
    -> Maybe SlotInterval
    -> [MarloweClientInput]
    -> Contract MarloweContractState MarloweSchema e MarloweData
applyInputs params typedValidator slotInterval inputs = mapError (review _MarloweError) $ do
    slotRange <- case slotInterval of
            Just si -> pure si
            Nothing -> do
                slot <- currentTime
                pure (slot, slot + defaultTxValidationRange)
    mkStep params typedValidator slotRange inputs

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
    maybeState <- getOnChainState typedValidator
    case maybeState of
        Just (OnChainState{ocsTxOutRef}, _) -> do
            let marloweData = toMarloweState ocsTxOutRef
            pure $ Just (params, marloweData)
        Nothing -> pure Nothing

{-| Get the current on-chain state of the state machine instance.
    Return Nothing if there is no state on chain.
    Throws an @SMContractError@ if the number of outputs at the machine address is greater than one.
-}
getOnChainState ::
    SmallTypedValidator
    -> Contract w schema MarloweError (Maybe (OnChainState, Map Ledger.TxOutRef Tx.ChainIndexTxOut))
getOnChainState validator = do
    (outRefs, utxos) <- mapError (review _MarloweError) $ marloweUtxoStatesAt validator
    case outRefs of
        []       -> pure Nothing
        [outRef] -> pure $ Just (OnChainState outRef, utxos)
        _        -> throwing_ _AmbiguousOnChainState


mkStep ::
    MarloweParams
    -> SmallTypedValidator
    -> SlotInterval
    -> [MarloweClientInput]
    -> Contract w MarloweSchema MarloweError MarloweData
mkStep MarloweParams{..} typedValidator slotInterval@(minSlot, maxSlot) clientInputs = do
    let
      times =
        Interval.Interval
          (Interval.LowerBound (Interval.Finite minSlot) True )
          (Interval.UpperBound (Interval.Finite maxSlot) False)
      range' =
        posixTimeRangeToContainedSlotRange
          unsafeGetSlotConfig
          times
    maybeState <- getOnChainState typedValidator
    case maybeState of
        Nothing -> throwError OnChainStateNotFound
        Just (OnChainState{ocsTxOutRef}, utxo) -> do
            let currentState = toMarloweState ocsTxOutRef
            let marloweTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef

            (allConstraints, marloweData) <- evaluateTxContstraints currentState times marloweTxOutRef

            pk <- Contract.ownPaymentPubKeyHash
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                    <> Constraints.unspentOutputs utxo
            let lookups:: ScriptLookups TypedMarloweValidator
                lookups = lookups1 { Constraints.slOwnPaymentPubKeyHash = Just pk }
            utx <- either (throwing _ConstraintResolutionError)
                        pure
                        (Constraints.mkTx lookups allConstraints)
            let utx' = utx
                        {
                            unBalancedTxTx = (unBalancedTxTx utx) {Tx.txValidRange = range'}
                        , unBalancedTxValidityTimeRange = times
                        }
            submitTxConfirmed $ Constraints.adjustUnbalancedTx utx'
            pure marloweData
  where
    evaluateTxContstraints :: MarloweData
        -> Ledger.POSIXTimeRange
        -> Tx.TxOutRef
        -> Contract w MarloweSchema MarloweError (TxConstraints [MarloweTxInput] MarloweData, MarloweData)
    evaluateTxContstraints MarloweData{..} times marloweTxOutRef = do
        let (inputs, inputsConstraints) = foldMap clientInputToInputAndConstraints clientInputs
        let txInput = TransactionInput {
                txInterval = slotInterval,
                txInputs = inputs }

        case computeTransaction txInput marloweState marloweContract of
            TransactionOutput {txOutPayments, txOutState, txOutContract} -> do
                let marloweData = MarloweData {
                        marloweContract = txOutContract,
                        marloweState = txOutState }
                let allConstraints = let
                        ownInputsConstraints =
                            [ InputConstraint
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
                                    [ OutputConstraint
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



waitForUpdateTimeout ::
    forall t w schema.
       SmallTypedValidator
    -> Promise w schema MarloweError t -- ^ The timeout
    -> Contract w schema MarloweError (Promise w schema MarloweError (WaitingResult t))
waitForUpdateTimeout typedValidator timeout = do
    let addr = validatorAddress typedValidator
    currentState <- getOnChainState typedValidator
    let success = case currentState of
            Nothing ->
                -- There is no on-chain state, so we wait for an output to appear
                -- at the address. Any output that appears needs to be checked
                -- with scChooser'
                promiseBind (utxoIsProduced addr) $ \txns -> do
                    produced <- concatMapM (marloweHistoryFrom typedValidator) $ NonEmpty.toList txns
                    case produced of
                        -- empty list shouldn't be possible, because we're waiting for txns with OnChainState
                        [history] -> pure $ Transition history
                        _         -> throwing_ _AmbiguousOnChainState
            Just (OnChainState{ocsTxOutRef=Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef}}, _) ->
                promiseBind (utxoIsSpent tyTxOutRefRef) $ \txn -> do
                    spent <- marloweHistoryFrom typedValidator txn
                    case spent of
                        [history] -> pure $ Transition history
                        _         -> throwing_ _UnableToExtractTransition
    pure $ select success (Timeout <$> timeout)


waitForUpdateUntilSlot ::
    forall w schema.
       SmallTypedValidator
    -> POSIXTime
    -> Contract w schema MarloweError (WaitingResult POSIXTime)
waitForUpdateUntilSlot client timeout =
    awaitPromise
      =<< waitForUpdateTimeout client (isTime timeout)


getInput ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> ChainIndexTx
    -> Maybe i
getInput outRef tx = do
    (_validator, Ledger.Redeemer r, _) <- listToMaybe $ mapMaybe Tx.inScripts $ filter (\Tx.TxIn{Tx.txInRef} -> outRef == txInRef) $ Set.toList $ _citxInputs tx
    PlutusTx.fromBuiltinData r
