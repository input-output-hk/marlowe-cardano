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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Scripts where
import           Data.Default                     (Default (def))
import           Language.Marlowe.Semantics
import           Language.Marlowe.SemanticsTypes  hiding (Contract)
import           Ledger
import           Ledger.Ada                       (adaSymbol)
import           Ledger.Constraints
import           Ledger.Constraints.OnChain
import           Ledger.Constraints.TxConstraints
import qualified Ledger.Interval                  as Interval
import           Ledger.Scripts                   (Validator)
import qualified Ledger.TimeSlot                  as TimeSlot
import qualified Ledger.Typed.Scripts             as Scripts
import qualified Ledger.Value                     as Val
import           Plutus.Contract.StateMachine     (StateMachine (..), Void)
import qualified Plutus.Contract.StateMachine     as SM
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap
import qualified PlutusTx.Prelude                 as P

type MarloweSlotRange = (Slot, Slot)
type MarloweInput = (MarloweSlotRange, [Input])


rolePayoutScript :: CurrencySymbol -> Validator
rolePayoutScript symbol = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode symbol)
  where
    wrapped s = Scripts.wrapValidator (rolePayoutValidator s)


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
rolePayoutValidator currency role _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role P.> 0


mkRolePayoutValidatorHash :: CurrencySymbol -> ValidatorHash
mkRolePayoutValidatorHash symbol = validatorHash (rolePayoutScript symbol)


defaultRolePayoutValidatorHash :: ValidatorHash
defaultRolePayoutValidatorHash = mkRolePayoutValidatorHash adaSymbol

{-# INLINABLE mkMarloweStateMachineTransition #-}
mkMarloweStateMachineTransition
    :: MarloweParams
    -> SM.State MarloweData
    -> MarloweInput
    -> Maybe (TxConstraints Void Void, SM.State MarloweData)
mkMarloweStateMachineTransition params SM.State{ SM.stateData=MarloweData{..}, SM.stateValue=scriptInValue}
    (interval@(minSlot, maxSlot), inputs) = do
    let positiveBalances = validateBalances marloweState ||
            -- Avoid creating a too-big string literal
            P.traceError ("M1")

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsConstraints = validateInputs params inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = inputBalance == scriptInValue

    let preconditionsOk = P.traceIfFalse "M2" $ positiveBalances && balancesOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (outputsConstraints, finalBalance) = let
                    payoutsByParty = AssocMap.toList $ P.foldMap payoutByParty txOutPayments
                    in case txOutContract of
                        Close -> (payoutConstraints payoutsByParty, P.zero)
                        _ -> let
                            outputsConstraints = payoutConstraints payoutsByParty
                            totalIncome = P.foldMap collectDeposits inputs
                            totalPayouts = P.foldMap snd payoutsByParty
                            finalBalance = inputBalance P.+ totalIncome P.- totalPayouts
                            in (outputsConstraints, finalBalance)
            -- TODO Push this use of time further down the code
            let range = TimeSlot.slotRangeToPOSIXTimeRange def $ Interval.interval minSlot maxSlot
            let constraints = inputsConstraints <> outputsConstraints <> mustValidateIn range
            if preconditionsOk
            then Just (constraints, SM.State marloweData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    validateInputs :: MarloweParams -> [Input] -> TxConstraints Void Void
    validateInputs MarloweParams{rolesCurrency} inputs = let
        (keys, roles) = P.foldMap validateInputWitness inputs
        mustSpendSetOfRoleTokens = P.foldMap mustSpendRoleToken (AssocMap.keys roles)
        in P.foldMap mustBeSignedBy keys P.<> mustSpendSetOfRoleTokens
      where
        validateInputWitness :: Input -> ([PubKeyHash], AssocMap.Map TokenName ())
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> (P.mempty, P.mempty)
          where
            validatePartyWitness (PK pk)     = ([pk], P.mempty)
            validatePartyWitness (Role role) = ([], AssocMap.singleton role ())

        mustSpendRoleToken :: TokenName -> TxConstraints Void Void
        mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1

    collectDeposits :: Input -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> TxConstraints i0 o0
    payoutConstraints payoutsByParty = P.foldMap payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value


{-# INLINABLE isFinal #-}
isFinal :: MarloweData -> Bool
isFinal MarloweData{marloweContract=c} = isClose c

{-# INLINABLE mkValidator #-}
mkValidator :: MarloweParams -> Scripts.ValidatorType MarloweStateMachine
mkValidator p = SM.mkValidator $ SM.mkStateMachine Nothing (mkMarloweStateMachineTransition p) isFinal

instance Scripts.ValidatorTypes (MarloweData) where
    type instance RedeemerType (MarloweData) = [Input]
    type instance DatumType (MarloweData) = MarloweData



{-# INLINABLE smallMarloweValidator #-}
smallMarloweValidator
    :: MarloweParams
    -> MarloweData
    -> [Input]
    -> ScriptContext
    -> Bool
smallMarloweValidator params MarloweData{..} inputs ctx = do
    let slotConfig = def :: TimeSlot.SlotConfig
    let txInfo = scriptContextTxInfo ctx
    let ownInput = case findOwnInput ctx of
            Just i -> i
            _      -> P.traceError "S0"
    let scriptInValue = maybe (P.traceError "S0" {-"Can't find validation input"-}) (txOutValue . txInInfoResolved) (findOwnInput ctx)
    let (minTime, maxTime) =
            case txInfoValidRange txInfo of
                Interval.Interval (Interval.LowerBound (Interval.Finite l) True) (Interval.UpperBound (Interval.Finite h) False) -> (l, h)
                _ -> P.traceError "Mr"
    let timeToSlot = TimeSlot.posixTimeToEnclosingSlot slotConfig
    let minSlot = timeToSlot minTime
    let maxSlot = timeToSlot maxTime
    let interval = (minSlot, maxSlot)
    let positiveBalances = validateBalances marloweState ||
            -- Avoid creating a too-big string literal
            P.traceError ("M1")

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsOk = validateInputs params txInfo inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = inputBalance P.== scriptInValue

    let preconditionsOk = P.traceIfFalse "M2" $ positiveBalances && balancesOk && inputsOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    -- let computedResult = TransactionOutput [] [] (emptyState minSlot) Close
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do
            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

                payoutsByParty = AssocMap.toList $ P.foldMap payoutByParty txOutPayments
                payoutsOk = payoutConstraints txInfo payoutsByParty
                checkContinuation = case txOutContract of
                    Close -> True
                    _ -> let
                        totalIncome = P.foldMap collectDeposits inputs
                        totalPayouts = P.foldMap snd payoutsByParty
                        finalBalance = inputBalance P.+ totalIncome P.- totalPayouts
                        outConstrs = OutputConstraint
                                    { ocDatum = marloweData
                                    , ocValue = finalBalance
                                    }
                        in checkOwnOutputConstraint ctx outConstrs
            preconditionsOk && payoutsOk && checkContinuation
        Error TEAmbiguousSlotIntervalError -> P.traceError "E1"
        Error TEApplyNoMatchError -> P.traceError "E2"
        Error (TEIntervalError (InvalidInterval _)) -> P.traceError "E3"
        Error (TEIntervalError (IntervalInPastError _ _)) -> P.traceError "E4"
        Error TEUselessTransaction -> P.traceError "E5"

  where
    validateInputs :: MarloweParams -> TxInfo -> [Input] -> Bool
    validateInputs MarloweParams{rolesCurrency} scriptContextTxInfo inputs = P.all validateInputWitness inputs
      where
        validateInputWitness :: Input -> Bool
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> True
          where
            validatePartyWitness (PK pk)     = P.traceIfFalse "L4" $ scriptContextTxInfo `txSignedBy` pk
            validatePartyWitness (Role role) = P.traceIfFalse "L5" -- "Spent value not OK"
                                               $ Val.singleton rolesCurrency role 1 `Val.leq` valueSpent scriptContextTxInfo

    collectDeposits :: Input -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: TxInfo -> [(Party, Val.Value)] -> Bool
    payoutConstraints scriptContextTxInfo payoutsByParty = P.all payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> P.traceIfFalse "La" $ value `Val.leq` valuePaidTo scriptContextTxInfo pk
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                payoutScriptHash = rolePayoutValidatorHash params
                outs = txInfoOutputs scriptContextTxInfo
                hsh = findDatumHash dataValue scriptContextTxInfo
                addr = Ledger.scriptHashAddress payoutScriptHash
                checkOutput TxOut{txOutAddress, txOutValue, txOutDatumHash=Just svh} =
                    txOutValue P.== value && hsh P.== Just svh && txOutAddress P.== addr
                checkOutput _ = False
                in P.traceIfFalse "Lb" -- "MustPayToOtherScript"
                $ any checkOutput outs



mkMarloweValidatorCode
    :: MarloweParams
    -> PlutusTx.CompiledCode (Scripts.ValidatorType MarloweStateMachine)
mkMarloweValidatorCode params =
    $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params


type MarloweStateMachine = StateMachine MarloweData MarloweInput

typedValidator :: MarloweParams -> Scripts.TypedValidator MarloweStateMachine
typedValidator params = Scripts.mkTypedValidator @MarloweStateMachine
    (mkMarloweValidatorCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarloweData @MarloweInput


typedValidator1 :: MarloweParams -> Scripts.TypedValidator MarloweData
typedValidator1 params = Scripts.mkTypedValidatorParam @MarloweData
    $$(PlutusTx.compile [|| smallMarloweValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    params
    where
        wrap = Scripts.wrapValidator


mkMachineInstance :: MarloweParams -> SM.StateMachineInstance MarloweData MarloweInput
mkMachineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine Nothing (mkMarloweStateMachineTransition params) isFinal)
    (typedValidator params)


mkMarloweClient :: MarloweParams -> SM.StateMachineClient MarloweData MarloweInput
mkMarloweClient params = SM.mkStateMachineClient (mkMachineInstance params)


defaultTxValidationRange :: Slot
defaultTxValidationRange = 10
