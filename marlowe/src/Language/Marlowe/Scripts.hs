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
{-# LANGUAGE NoImplicitPrelude     #-}
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
import GHC.Generics
import Language.Marlowe.Pretty (Pretty (..))
import Language.Marlowe.Semantics
import Language.Marlowe.SemanticsTypes
import Ledger
import Ledger.Ada (adaSymbol)
import Ledger.Constraints.OnChain
import Ledger.Constraints.TxConstraints
import qualified Ledger.Interval as Interval
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Val
import PlutusTx (makeIsDataIndexed, makeLift)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude
import qualified Prelude as Haskell
import Unsafe.Coerce

type MarloweSlotRange = (Slot, Slot)
type MarloweInput = [MarloweTxInput]

-- Yeah, I know
type SmallUntypedTypedValidator = Scripts.TypedValidator Scripts.Any
type SmallTypedValidator = Scripts.TypedValidator TypedMarloweValidator

data TypedMarloweValidator

{- Type instances for small typed Marlowe validator -}
instance Scripts.ValidatorTypes TypedMarloweValidator where
    type instance RedeemerType TypedMarloweValidator = MarloweInput
    type instance DatumType TypedMarloweValidator = MarloweData


data MarloweTxInput = Input InputContent
                    | MerkleizedTxInput InputContent BuiltinByteString
  deriving stock (Haskell.Show,Haskell.Eq,Generic)
  deriving anyclass (Pretty)


rolePayoutScript :: CurrencySymbol -> Validator
rolePayoutScript symbol = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode symbol)
  where
    wrapped s = Scripts.wrapValidator (rolePayoutValidator s)


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
rolePayoutValidator currency role _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role > 0


mkRolePayoutValidatorHash :: CurrencySymbol -> ValidatorHash
mkRolePayoutValidatorHash symbol = validatorHash (rolePayoutScript symbol)


defaultRolePayoutValidatorHash :: ValidatorHash
defaultRolePayoutValidatorHash = mkRolePayoutValidatorHash adaSymbol


{-# INLINABLE smallMarloweValidator #-}
smallMarloweValidator
    :: MarloweParams
    -> MarloweData
    -> MarloweInput
    -> ScriptContext
    -> Bool
smallMarloweValidator MarloweParams{rolesCurrency, rolePayoutValidatorHash, slotConfig =
    (scSlotLength, scSlotZeroTime)}
    MarloweData{..}
    marloweTxInputs
    ctx@ScriptContext{scriptContextTxInfo} = do

    let ownInput = case findOwnInput ctx of
            Just i -> i
            _      -> traceError "I0" {-"Can't find validation input"-}
    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let interval =
            case TimeSlot.posixTimeRangeToContainedSlotRange TimeSlot.SlotConfig{..} $ txInfoValidRange scriptContextTxInfo of
                -- FIXME: Recheck this, but it appears that any inclusiveness can appear at either bound when milliseconds
                --        of POSIX time is converted from slot number.
                Interval.Interval (Interval.LowerBound (Interval.Finite l) _) (Interval.UpperBound (Interval.Finite h) _) -> (l, h)
                _ -> traceError "R0"
    let positiveBalances = traceIfFalse "B0" $ validateBalances marloweState

    {- Find Contract continuation in TxInfo datums by hash or fail with error -}
    let inputs = fmap marloweTxInputToInput marloweTxInputs
    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsOk = validateInputs inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = traceIfFalse "B1" $ inputBalance == scriptInValue

    let preconditionsOk = positiveBalances && balancesOk

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

                payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
                payoutsOk = payoutConstraints payoutsByParty
                checkContinuation = case txOutContract of
                    Close -> True
                    _ -> let
                        totalIncome = foldMap (collectDeposits . getInputContent) inputs
                        totalPayouts = foldMap snd payoutsByParty
                        finalBalance = inputBalance + totalIncome - totalPayouts
                        outConstrs = OutputConstraint
                                    { ocDatum = marloweData
                                    , ocValue = finalBalance
                                    }
                        in checkOwnOutputConstraint ctx outConstrs
            preconditionsOk && inputsOk && payoutsOk && checkContinuation
        Error TEAmbiguousSlotIntervalError -> traceError "E1"
        Error TEApplyNoMatchError -> traceError "E2"
        Error (TEIntervalError (InvalidInterval _)) -> traceError "E3"
        Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "E4"
        Error TEUselessTransaction -> traceError "E5"
        Error TEHashMismatch -> traceError "E6"

  where
    checkScriptOutput addr hsh value TxOut{txOutAddress, txOutValue, txOutDatumHash=Just svh} =
                    txOutValue == value && hsh == Just svh && txOutAddress == addr
    checkScriptOutput _ _ _ _ = False

    allOutputs :: [TxOut]
    allOutputs = txInfoOutputs scriptContextTxInfo

    marloweTxInputToInput :: MarloweTxInput -> Input
    marloweTxInputToInput (MerkleizedTxInput input hash) =
        case findDatum (DatumHash hash) scriptContextTxInfo of
            Just (Datum d) -> let
                continuation = PlutusTx.unsafeFromBuiltinData d
                in MerkleizedInput input hash continuation
            Nothing -> traceError "H"
    marloweTxInputToInput (Input input) = NormalInput input

    validateInputs :: [Input] -> Bool
    validateInputs inputs = all (validateInputWitness . getInputContent) inputs
      where
        validateInputWitness :: InputContent -> Bool
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> True
          where
            validatePartyWitness (PK pk)     = traceIfFalse "S" $ scriptContextTxInfo `txSignedBy` pk
            validatePartyWitness (Role role) = traceIfFalse "T" -- "Spent value not OK"
                                               $ Val.singleton rolesCurrency role 1 `Val.leq` valueSpent scriptContextTxInfo

    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> Bool
    payoutConstraints payoutsByParty = all payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> traceIfFalse "P" $ value `Val.leq` valuePaidTo scriptContextTxInfo pk
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                hsh = findDatumHash dataValue scriptContextTxInfo
                addr = Ledger.scriptHashAddress rolePayoutValidatorHash
                in traceIfFalse "R" $ any (checkScriptOutput addr hsh value) allOutputs


smallTypedValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
smallTypedValidator = Scripts.mkTypedValidatorParam @TypedMarloweValidator
    $$(PlutusTx.compile [|| smallMarloweValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator


smallUntypedValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
smallUntypedValidator params = let
    wrapped s = Scripts.wrapValidator (smallMarloweValidator s)
    typed = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    -- Yeah, I know. It works, though.
    -- Remove this when Typed Validator has the same size as untyped.
    in unsafeCoerce (Scripts.unsafeMkTypedValidator typed)


defaultTxValidationRange :: Slot
defaultTxValidationRange = 10

marloweTxInputFromInput :: Input -> MarloweTxInput
marloweTxInputFromInput (NormalInput i)         = Input i
marloweTxInputFromInput (MerkleizedInput i h _) = MerkleizedTxInput i h

marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = fmap marloweTxInputFromInput

makeLift ''MarloweTxInput
makeIsDataIndexed ''MarloweTxInput [('Input,0),('MerkleizedTxInput,1)]
