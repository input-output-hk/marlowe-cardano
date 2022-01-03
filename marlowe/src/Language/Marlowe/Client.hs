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
import           Control.Lens
import           Control.Monad                    (forM_, void)
import           Control.Monad.Error.Lens         (catching, throwing, throwing_)
import           Data.Aeson                       (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Default                     (def)
import           Data.Either                      (rights)
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (isNothing, listToMaybe, mapMaybe, maybeToList)
import           Data.Monoid                      (First (..))
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.UUID                        (UUID)
import           Data.Void                        (absurd)
import           GHC.Generics                     (Generic)
import           Language.Marlowe.Scripts
import           Language.Marlowe.Semantics
import qualified Language.Marlowe.Semantics       as Marlowe
import           Language.Marlowe.SemanticsTypes  hiding (Contract, getAction)
import qualified Language.Marlowe.SemanticsTypes  as Marlowe
import           Language.Marlowe.Util            (extractNonMerkleizedContractRoles)
import           Ledger                           (CurrencySymbol, Datum (..), PubKeyHash, Slot (..), TokenName,
                                                   TxOut (..), TxOutRef, inScripts, txOutValue)
import qualified Ledger
import           Ledger.Ada                       (adaSymbol, adaToken, adaValueOf, lovelaceValueOf)
import           Ledger.Address                   (pubKeyHashAddress, scriptHashAddress)
import           Ledger.Constraints
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints
import qualified Ledger.Interval                  as Interval
import           Ledger.Scripts                   (datumHash, unitRedeemer)
import           Ledger.TimeSlot                  (SlotConfig (..))
import qualified Ledger.Tx                        as Tx
import           Ledger.Typed.Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Tx                  (TypedScriptTxOut (..), tyTxOutData)
import qualified Ledger.Typed.Tx                  as Typed
import qualified Ledger.Value                     as Val
import           Plutus.ChainIndex                (ChainIndexTx (..), _ValidTx, citxInputs, citxOutputs, citxTxId)
import           Plutus.Contract                  as Contract
import           Plutus.Contract.StateMachine     (AsSMContractError (..), Void)
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contract.Wallet           (getUnspentOutput)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap


type MarloweSchema =
        Endpoint "create" (UUID, AssocMap.Map Val.TokenName PubKeyHash, Marlowe.Contract)
        .\/ Endpoint "apply-inputs" (UUID, MarloweParams, Maybe SlotInterval, [Input])
        .\/ Endpoint "auto" (UUID, MarloweParams, Party, Slot)
        .\/ Endpoint "redeem" (UUID, MarloweParams, TokenName, PubKeyHash)
        .\/ Endpoint "close" UUID


type MarloweCompanionSchema = EmptySchema
type MarloweFollowSchema = Endpoint "follow" MarloweParams


data MarloweError =
    StateMachineError SM.SMContractError
    | TransitionError
    | AmbiguousOnChainState
    | OnChainStateNotFound
    | MarloweEvaluationError TransactionError
    | OtherContractError ContractError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


makeClassyPrisms ''MarloweError

instance AsSMContractError MarloweError where
    _SMContractError = _StateMachineError

instance AsContractError MarloweError where
    _ContractError = _OtherContractError

instance AsCheckpointError MarloweError where
    _CheckpointError = _OtherContractError . _CheckpointError

data PartyAction
             = PayDeposit AccountId Party Token Integer
             | WaitForTimeout Slot
             | WaitOtherActionUntil Slot
             | NotSure
             | CloseContract
  deriving (Show)

type RoleOwners = AssocMap.Map Val.TokenName PubKeyHash

data ContractHistory =
    ContractHistory
        { chParams  :: First (MarloweParams, MarloweData)
        , chHistory :: [TransactionInput]
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ContractHistory)


data OnChainState =
    OnChainState
        { ocsTxOut    :: Typed.TypedScriptTxOut TypedMarloweValidator -- ^ Typed transaction output
        , ocsTxOutRef :: Typed.TypedScriptTxOutRef TypedMarloweValidator -- ^ Typed UTXO
        , ocsTx       :: ChainIndexTx -- ^ Transaction that produced the output
        }


-- | The outcome of 'waitForUpdateTimeout'
data WaitingResult t i s
    = Timeout t -- ^ The timeout happened before any change of the on-chain state was detected
    | ContractEnded ChainIndexTx i -- ^ The state machine instance ended
    | Transition ChainIndexTx i s -- ^ The state machine instance transitioned to a new state
    | InitialState ChainIndexTx s -- ^ The state machine instance was initialised
  deriving stock (Show,Generic,Functor)
  deriving anyclass (ToJSON, FromJSON)


created :: MarloweParams -> MarloweData -> ContractHistory
created p d = mempty{chParams = First (Just (p, d)) }

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

data LastResult = OK UUID EndpointName | SomeError UUID EndpointName MarloweError | Unknown
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup LastResult where
    any <> Unknown = any
    _ <> last      = last

instance Monoid LastResult where
    mempty = Unknown

type MarloweContractState = LastResult


mkMarloweTypedValidator :: MarloweParams -> SmallTypedValidator
mkMarloweTypedValidator = smallUntypedValidator


minLovelaceDeposit :: Integer
minLovelaceDeposit = 2000000


marloweFollowContract :: Contract ContractHistory MarloweFollowSchema MarloweError ()
marloweFollowContract = awaitPromise $ endpoint @"follow" $ \params -> do
    let typedValidator = mkMarloweTypedValidator params
    let go [] = pure InProgress
        go (tx:rest) = do
            res <- updateHistoryFromTx typedValidator params tx
            case res of
                Finished   -> pure Finished
                InProgress -> go rest

    go [] >>= checkpointLoop (follow typedValidator params)

  where
    follow typedValidator params = \case
        Finished -> do
            logDebug @String ("Contract finished " <> show params)
            pure (Right InProgress) -- do not close the contract so we can see it in Marlowe Run history
        InProgress -> do
            result <- waitForUpdateTimeout typedValidator never >>= awaitPromise
            case result of
                Timeout t -> absurd t
                ContractEnded tx inputs -> do
                    interval <- case _citxValidRange tx of
                        Interval.Interval (Interval.LowerBound (Interval.Finite l) True) (Interval.UpperBound (Interval.Finite h) False) -> pure (l, h)
                        _ -> throwError (OtherContractError $ OtherError "Interval")
                    tell @ContractHistory (transition $ TransactionInput interval inputs)
                    pure (Right Finished)
                Transition tx inputs _ -> do
                    interval <- case _citxValidRange tx of
                        Interval.Interval (Interval.LowerBound (Interval.Finite l) True) (Interval.UpperBound (Interval.Finite h) False) -> pure (l, h)
                        _ -> throwError (OtherContractError $ OtherError "Interval")
                    tell @ContractHistory (transition $ TransactionInput interval inputs)
                    pure (Right InProgress)
                InitialState _ OnChainState{ocsTxOut} -> do
                    let initialMarloweData = tyTxOutData ocsTxOut
                    tell @ContractHistory (created params initialMarloweData)
                    pure (Right InProgress)

    updateHistoryFromTx :: TypedValidator TypedMarloweValidator
                        -> MarloweParams
                        -> ChainIndexTx
                        -> Contract ContractHistory MarloweFollowSchema MarloweError ContractProgress
    updateHistoryFromTx inst params tx = do
        logInfo @String $ "Updating history from tx " <> show (view citxTxId tx)
        let address = Scripts.validatorAddress inst
        utxos <- fmap ( Map.filter ((==) address . view Ledger.ciTxOutAddress . fst)
                      . Map.fromList
                      ) $ utxosTxOutTxFromTx tx
        let states = getStates inst utxos
        case findInput inst tx of
            -- if there's no TxIn for Marlowe contract that means
            -- it's a contract creation transaction, and there is Marlowe TxOut
            Nothing -> case states of
                [OnChainState{ocsTxOut=state}] -> do
                    let initialMarloweData = tyTxOutData state
                    logInfo @String ("Contract created " <> show initialMarloweData)
                    tell $ created params initialMarloweData
                    pure InProgress
                _    -> throwError (OtherContractError $ OtherError "CCC")
            -- There is TxIn with Marlowe contract, hence this is a state transition
            Just inputs -> do
                interval <- case _citxValidRange tx of
                        Interval.Interval (Interval.LowerBound (Interval.Finite l) True) (Interval.UpperBound (Interval.Finite h) False) -> pure (l, h)
                        _ -> throwError (OtherContractError $ OtherError "Interval")
                let txInput = TransactionInput {
                        txInterval = interval,
                        txInputs = inputs }
                tell $ transition txInput
                case states of
                    -- when there is no Marlowe TxOut the contract is closed
                    -- and we can close the follower contract
                    [] -> pure Finished
                    -- otherwise we continue following
                    _  -> pure InProgress

    findInput inst tx = do
        let inputs = Set.toList (view citxInputs tx) >>= maybeToList . inScripts
        let script = Scripts.validatorScript inst
        -- find previous Marlowe contract
        let marloweTxInputs = filter (\(validator, _, _) -> validator == script) inputs
        case marloweTxInputs of
            []                          -> Nothing
            [(_, Ledger.Redeemer d, _)] -> PlutusTx.fromBuiltinData d
            _                           -> Nothing

{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = selectList [create, apply, auto, redeem, close]
  where
    catchError reqId endpointName handler = catching _MarloweError
        (void $ mapError (review _MarloweError) handler)
        (\er -> do
            logWarn @String (show er)
            tell $ SomeError reqId endpointName er
            marlowePlutusContract)
    create = endpoint @"create" $ \(reqId, owners, contract) -> catchError reqId "create" $ do
        -- Create a transaction with the role tokens and pay them to the contract creator
        -- See Note [The contract is not ready]
        ownPubKey <- Contract.ownPubKeyHash
        (params, distributeRoleTokens, lkps) <- setupMarloweParams owners contract
        slot <- currentSlot
        time <- Ledger.getPOSIXTime <$> currentTime
        let timeZero = Ledger.POSIXTime $ time - getSlot slot * scSlotLength def
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = State
                    { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) minLovelaceDeposit
                    , choices  = AssocMap.empty
                    , boundValues = AssocMap.empty
                    , minSlot = slot },
                slotConfigFix = (scSlotLength def, timeZero) }  -- FIXME: This is a temporary fix until SCP-3150 is complete.
        let minAdaTxOut = lovelaceValueOf minLovelaceDeposit
        let typedValidator = mkMarloweTypedValidator params
        let tx = mustPayToTheScript marloweData minAdaTxOut <> distributeRoleTokens
        let lookups = Constraints.typedValidatorLookups typedValidator <> lkps
        -- Create the Marlowe contract and pay the role tokens to the owners
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
        submitTxConfirmed utx
        tell $ OK reqId "create"
        marlowePlutusContract
    apply = endpoint @"apply-inputs" $ \(reqId, params, slotInterval, inputs) -> catchError reqId "apply-inputs" $ do
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator slotInterval inputs
        tell $ OK reqId "apply-inputs"
        marlowePlutusContract
    redeem = promiseMap (mapError (review _MarloweError)) $ endpoint @"redeem" $ \(reqId, MarloweParams{rolesCurrency}, role, pkh) -> catchError reqId "redeem" $ do
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        utxos <- utxosAt address
        let spendPayoutConstraints tx ref txout = let
                expectedDatumHash = datumHash (Datum $ PlutusTx.toBuiltinData role)
                amount = view Ledger.ciTxOutValue txout
                dh = either id Ledger.datumHash <$> preview Ledger.ciTxOutDatum txout
                in case dh of
                    Just datumHash | datumHash == expectedDatumHash ->
                        -- we spend the rolePayoutScript address
                        Constraints.mustSpendScriptOutput ref unitRedeemer
                        -- and pay to a token owner
                            <> Constraints.mustPayToPubKey pkh amount
                    _ -> tx

        let spendPayouts = Map.foldlWithKey spendPayoutConstraints mempty utxos
        if spendPayouts == mempty
        then do
            tell $ OK reqId "redeem"
        else do
            let
              constraints = spendPayouts
                  -- must spend a role token for authorization
                  <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
              -- lookup for payout validator and role payouts
              validator = rolePayoutScript rolesCurrency
              lookups = Constraints.otherScript validator
                  <> Constraints.unspentOutputs utxos
                  <> Constraints.ownPubKeyHash pkh
            tx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx @Void lookups constraints)
            _ <- submitUnbalancedTx tx
            tell $ OK reqId "redeem"

        marlowePlutusContract
    auto = endpoint @"auto" $ \(reqId, params, party, untilSlot) -> catchError reqId "auto" $ do
        let typedValidator = mkMarloweTypedValidator params
        let continueWith :: MarloweData -> Contract MarloweContractState MarloweSchema MarloweError ()
            continueWith md@MarloweData{marloweContract} =
                if canAutoExecuteContractForParty party marloweContract
                then autoExecuteContract reqId params typedValidator party md
                else do
                    tell $ OK reqId "auto"
                    marlowePlutusContract

        maybeState <- getOnChainState typedValidator
        case maybeState of
            Nothing -> do
                wr <- waitForUpdateUntilSlot @[Input] typedValidator untilSlot
                case wr of
                    ContractEnded{} -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        tell $ OK reqId "auto"
                        marlowePlutusContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        tell $ OK reqId "auto"
                        marlowePlutusContract
                    Transition _ _ marloweData -> continueWith marloweData
                    InitialState _ marloweData -> continueWith marloweData
            Just (OnChainState{ocsTxOut=st}, _) -> do
                let marloweData = tyTxOutData st
                continueWith marloweData
    -- The MarloweApp contract is closed implicitly by not returning
    -- itself (marlowePlutusContract) as a continuation
    close = endpoint @"close" $ \reqId -> tell $ OK reqId "close"


    autoExecuteContract :: UUID
                      -> MarloweParams
                      -> SmallTypedValidator
                      -> Party
                      -> MarloweData
                      -> Contract MarloweContractState MarloweSchema MarloweError ()
    autoExecuteContract reqId params typedValidator party marloweData = do
        slot <- currentSlot
        let slotRange = (slot, slot + defaultTxValidationRange)
        let action = getAction slotRange party marloweData
        case action of
            PayDeposit acc p token amount -> do
                logInfo @String $ "PayDeposit " <> show amount <> " at within slots " <> show slotRange
                let payDeposit = do
                        marloweData <- mkStep params typedValidator slotRange [NormalInput $ IDeposit acc p token amount]
                        continueWith marloweData
                catching _MarloweError payDeposit $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 slots"
                    _ <- awaitSlot (slot + 2)
                    continueWith marloweData
            WaitForTimeout timeout -> do
                logInfo @String $ "WaitForTimeout " <> show timeout
                _ <- awaitSlot timeout
                continueWith marloweData
            WaitOtherActionUntil timeout -> do
                logInfo @String $ "WaitOtherActionUntil " <> show timeout
                wr <- waitForUpdateUntilSlot @[Input] typedValidator timeout
                case wr of
                    ContractEnded{} -> do
                        logInfo @String $ "Contract Ended"
                        tell $ OK reqId "auto"
                        marlowePlutusContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith marloweData
                    Transition _ _ marloweData -> continueWith marloweData
                    InitialState _ marloweData -> continueWith marloweData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                let closeContract = do
                        _ <- mkStep params typedValidator slotRange []
                        tell $ OK reqId "auto"
                        marlowePlutusContract

                catching _MarloweError closeContract $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry CloseContract in 2 slots"
                    _ <- awaitSlot (slot + 2)
                    continueWith marloweData
            NotSure -> do
                logInfo @String $ "NotSure"
                tell $ OK reqId "auto"
                marlowePlutusContract

          where
            continueWith = autoExecuteContract reqId params typedValidator party


setupMarloweParams
    :: forall s e i o a.
    (AsMarloweError e)
    => RoleOwners
    -> Marlowe.Contract
    -> Contract MarloweContractState s e
        (MarloweParams, TxConstraints i o, ScriptLookups a)
setupMarloweParams owners contract = mapError (review _MarloweError) $ do
    ownAddress <- pubKeyHashAddress <$> Contract.ownPubKeyHash
    let roles = extractNonMerkleizedContractRoles contract
    if Set.null roles
    then do
        let params = MarloweParams
                { rolesCurrency = adaSymbol
                , rolePayoutValidatorHash = defaultRolePayoutValidatorHash }
        pure (params, mempty, mempty)
    else if roles `Set.isSubsetOf` Set.fromList (AssocMap.keys owners)
    then do
        let tokens = fmap (\role -> (role, 1)) $ Set.toList roles
        txOutRef@(Ledger.TxOutRef h i) <- getUnspentOutput
        utxo <- utxosAt ownAddress
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
        let giveToParty (role, pkh) = Constraints.mustPayToPubKey pkh (Val.singleton rolesSymbol role 1 <> minAdaTxOut)
        let distributeRoleTokens = foldMap giveToParty (AssocMap.toList owners)
        let params = MarloweParams
                { rolesCurrency = rolesSymbol
                , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesSymbol }
        pure (params, mintTx <> distributeRoleTokens, lookups)
    else do
        let missingRoles = roles `Set.difference` Set.fromList (AssocMap.keys owners)
        let message = T.pack $ "You didn't specify owners of these roles: " <> show missingRoles
        throwing _ContractError $ OtherError message


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
                    When [cases] (Slot 100) (When [Case Deposit Close]] (Slot 105) Close)

                For a slot range (95, 105) we get RRAmbiguousSlotIntervalError
                because timeout 100 is inside the slot range.
                Now, we wait for slot 105, and we miss the Deposit.

                To avoid that we need to know what was the original timeout
                that caused RRAmbiguousSlotIntervalError (i.e. Slot 100).
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
    -> [Input]
    -> Contract MarloweContractState MarloweSchema e MarloweData
applyInputs params typedValidator slotInterval inputs = mapError (review _MarloweError) $ do
    slotRange <- case slotInterval of
            Just si -> pure si
            Nothing -> do
                slot <- currentSlot
                pure (slot, slot + defaultTxValidationRange)
    mkStep params typedValidator slotRange inputs

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rolesCurrency = MarloweParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency }


defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


newtype CompanionState = CompanionState (Map MarloweParams MarloweData)
  deriving (Semigroup,Monoid) via (Map MarloweParams MarloweData)

instance ToJSON CompanionState where
    toJSON (CompanionState m) = toJSON $ Map.toList m

instance FromJSON CompanionState where
    parseJSON v = CompanionState . Map.fromList <$> parseJSON v

{-|
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
        pkh <- Contract.ownPubKeyHash
        let ownAddress = pubKeyHashAddress pkh
        -- Filter those outputs for role tokens and notify the WebSocket subscribers
        -- NOTE: CombinedWSStreamToServer has an API to subscribe to WS notifications
        utxo <- utxosAt ownAddress
        let txOuts = fmap Ledger.toTxOut $ Map.elems utxo
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
                logDebug @String $ "Companion contract: Updating observable state"
                tell $ CompanionState (Map.singleton params md)
            Nothing           -> do
            -- The result will be empty if:
            --   * Note [The contract is not ready]: When you create a Marlowe contract first we create
            --                                       the role tokens, pay them to the contract creator and
            --                                       then we create the Marlowe contract.
            --   * If the marlowe contract is closed.
                -- TODO: Change for debug
                logWarn @String $ "Companion contract: On-chain state not found!"
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
        Just (OnChainState{ocsTxOut}, _) -> do
            let marloweData = tyTxOutData ocsTxOut
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
    utxoTx <- mapError (review _MarloweError) $ utxosTxOutTxAt (validatorAddress validator)
    let states = getStates validator utxoTx
    case states of
        []      -> pure Nothing
        [state] -> pure $ Just (state, fmap fst utxoTx)
        _       -> throwing_ _AmbiguousOnChainState


getStates :: SmallTypedValidator
    -> Map Tx.TxOutRef (Tx.ChainIndexTxOut, ChainIndexTx)
    -> [OnChainState]
getStates si refMap =
    let lkp (ref, (out, tx)) = do
            ocsTxOutRef <- Typed.typeScriptTxOutRef (\r -> fst <$> Map.lookup r refMap) si ref
            ocsTxOut <- Typed.typeScriptTxOut si ref out
            pure OnChainState{ocsTxOut, ocsTxOutRef, ocsTx = tx}
    in rights $ fmap lkp $ Map.toList refMap


mkStep ::
    MarloweParams
    -> SmallTypedValidator
    -> SlotInterval
    -> [Input]
    -> Contract w MarloweSchema MarloweError MarloweData
mkStep params typedValidator range input = do
    maybeState <- getOnChainState typedValidator
    case maybeState of
        Nothing -> throwError OnChainStateNotFound
        Just (onChainState, utxo) -> do
            let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=currentState, tyTxOutTxOut}, ocsTxOutRef} = onChainState
                oldState = SM.State
                    { SM.stateData = currentState
                    , SM.stateValue = Ledger.txOutValue tyTxOutTxOut
                    }
                inputConstraints = [InputConstraint{icRedeemer=input, icTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef }]

            case mkMarloweStateMachineTransition params oldState (range, input) of
                Just (newConstraints, newState)  -> do
                    let isFinal = isClose (marloweContract $ SM.stateData newState)
                        lookups1 =
                            Constraints.typedValidatorLookups typedValidator
                            <> Constraints.unspentOutputs utxo
                        outputConstraints =
                            [ OutputConstraint
                                { ocDatum = SM.stateData newState
                                , ocValue = SM.stateValue newState
                                }
                            | not isFinal ]
                    let smtConstraints = newConstraints
                                    { txOwnInputs = inputConstraints
                                    , txOwnOutputs = outputConstraints
                                    }
                    pk <- Contract.ownPubKeyHash
                    let lookups:: ScriptLookups TypedMarloweValidator
                        lookups = lookups1 { Constraints.slOwnPubkeyHash = Just pk }
                    utx <- either (throwing _ConstraintResolutionError)
                                pure
                                (Constraints.mkTx lookups smtConstraints)
                    submitTxConfirmed utx
                    pure (SM.stateData newState)
                Nothing -> throwError TransitionError


waitForUpdateTimeout ::
    forall i t w schema.
    (PlutusTx.FromData i
    )
    => SmallTypedValidator
    -> Promise w schema MarloweError t -- ^ The timeout
    -> Contract w schema MarloweError (Promise w schema MarloweError (WaitingResult t i OnChainState))
waitForUpdateTimeout typedValidator timeout = do
    let addr = validatorAddress typedValidator
    currentState <- getOnChainState typedValidator
    let success = case currentState of
            Nothing ->
                -- There is no on-chain state, so we wait for an output to appear
                -- at the address. Any output that appears needs to be checked
                -- with scChooser'
                promiseBind (utxoIsProduced addr) $ \txns -> do
                    outRefMaps <- traverse utxosTxOutTxFromTx txns
                    let produced = getStates typedValidator (Map.fromList $ concat outRefMaps)
                    case produced of
                        -- empty list shouldn't be possible, because we're waiting for txns with OnChainState
                        [onChainState] -> pure $ InitialState (ocsTx onChainState) onChainState
                        _              -> throwing_ _AmbiguousOnChainState
            Just (OnChainState{ocsTxOutRef=Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef}, ocsTx}, _) ->
                promiseBind (utxoIsSpent tyTxOutRefRef) $ \txn -> do
                    outRefMap <- Map.fromList <$> utxosTxOutTxFromTx txn
                    let newStates = getStates typedValidator outRefMap
                        inp       = getInput tyTxOutRefRef txn
                    case (newStates, inp) of
                        ([], Just i)         -> pure (ContractEnded ocsTx i)
                        ([newState], Just i) -> pure (Transition ocsTx i newState)
                        _                    -> throwing_ _UnableToExtractTransition
    pure $ select success (Timeout <$> timeout)


waitForUpdateUntilSlot ::
    forall i w schema.
    ( PlutusTx.FromData i
    )
    => SmallTypedValidator
    -> Slot
    -> Contract w schema MarloweError (WaitingResult Slot i MarloweData)
waitForUpdateUntilSlot client timeoutSlot = do
    result <- waitForUpdateTimeout client (isSlot timeoutSlot)
    let getTxOutData = fmap (fmap (tyTxOutData . ocsTxOut)) . awaitPromise
    getTxOutData result


getInput ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> ChainIndexTx
    -> Maybe i
getInput outRef tx = do
    (_validator, Ledger.Redeemer r, _) <- listToMaybe $ mapMaybe Tx.inScripts $ filter (\Tx.TxIn{Tx.txInRef} -> outRef == txInRef) $ Set.toList $ _citxInputs tx
    PlutusTx.fromBuiltinData r
