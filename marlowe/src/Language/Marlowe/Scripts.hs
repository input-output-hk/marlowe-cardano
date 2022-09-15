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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}


module Language.Marlowe.Scripts where
import GHC.Generics
import Language.Marlowe.Core.V1.Semantics as Semantics
import Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Pretty (Pretty(..))
import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Address as Address (scriptHashAddress)
import qualified Plutus.V1.Ledger.Value as Val
import Plutus.V2.Ledger.Api
  ( Address(Address)
  , Credential(..)
  , CurrencySymbol
  , Datum(Datum)
  , DatumHash(DatumHash)
  , Extended(..)
  , Interval(..)
  , LowerBound(..)
  , POSIXTime(..)
  , POSIXTimeRange
  , ScriptContext(ScriptContext, scriptContextPurpose, scriptContextTxInfo)
  , ScriptPurpose(Spending)
  , TokenName
  , TxInInfo(TxInInfo, txInInfoOutRef, txInInfoResolved)
  , TxInfo(TxInfo, txInfoInputs, txInfoOutputs, txInfoValidRange)
  , UpperBound(..)
  , ValidatorHash
  , mkValidatorScript
  )
import Plutus.V2.Ledger.Contexts (findDatum, findDatumHash, txSignedBy, valuePaidTo, valueSpent)
import Plutus.V2.Ledger.Tx (OutputDatum(OutputDatumHash), TxOut(TxOut, txOutAddress, txOutDatum, txOutValue))
import PlutusTx (makeIsDataIndexed, makeLift)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
-- Added to silence cabal "unused plutus-tx-plugin" warning
import Plutus.Script.Utils.V2.Typed.Scripts (mkTypedValidator, mkUntypedValidator)
import PlutusTx.Plugin ()
import PlutusTx.Prelude as PlutusTxPrelude
import qualified Prelude as Haskell
import Unsafe.Coerce (unsafeCoerce)

type MarloweTimeRange = (POSIXTime, POSIXTime)
type MarloweInput = [MarloweTxInput]

data TypedMarloweValidator

{- Type instances for small typed Marlowe validator -}
instance Scripts.ValidatorTypes TypedMarloweValidator where
    type instance RedeemerType TypedMarloweValidator = MarloweInput
    type instance DatumType TypedMarloweValidator = MarloweData

data TypedRolePayoutValidator

instance Scripts.ValidatorTypes TypedRolePayoutValidator where
  type instance RedeemerType TypedRolePayoutValidator = ()
  type instance DatumType TypedRolePayoutValidator = (CurrencySymbol, TokenName)


data MarloweTxInput = Input InputContent
                    | MerkleizedTxInput InputContent BuiltinByteString
  deriving stock (Haskell.Show,Haskell.Eq,Generic)
  deriving anyclass (Pretty)


rolePayoutValidator :: Scripts.TypedValidator TypedRolePayoutValidator
rolePayoutValidator = mkTypedValidator @TypedRolePayoutValidator
  $$(PlutusTx.compile [|| mkRolePayoutValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @(CurrencySymbol, TokenName) @()


{-# INLINABLE rolePayoutValidator #-}
mkRolePayoutValidator :: (CurrencySymbol, TokenName) -> () -> ScriptContext -> Bool
mkRolePayoutValidator (currency, role) _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role > 0

rolePayoutValidatorHash :: ValidatorHash
rolePayoutValidatorHash = Scripts.validatorHash rolePayoutValidator


{-# INLINABLE closeInterval #-}
closeInterval :: POSIXTimeRange -> Maybe (POSIXTime, POSIXTime)
closeInterval (Interval (LowerBound (Finite (POSIXTime l)) lc) (UpperBound (Finite (POSIXTime h)) hc)) =
  Just
    (
      POSIXTime $ l + 1 - fromEnum lc  -- Add one millisecond if the interval was open.
    , POSIXTime $ h - 1 + fromEnum hc  -- Subtract one millisecond if the interval was open.
    )
closeInterval _ = Nothing


{-# INLINABLE mkMarloweValidator #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
mkMarloweValidator
    :: ValidatorHash
    -> MarloweData
    -> MarloweInput
    -> ScriptContext
    -> Bool
mkMarloweValidator
    rolePayoutValidatorHash
    MarloweData{..}
    marloweTxInputs
    ctx@ScriptContext{scriptContextTxInfo} = do

    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let interval =
            -- Marlowe semantics require a closed interval, so we might adjust by one millisecond.
            case closeInterval $ txInfoValidRange scriptContextTxInfo of
                Just interval' -> interval'
                Nothing        -> traceError "R0"
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
                    marloweParams = marloweParams,
                    marloweContract = txOutContract,
                    marloweState = txOutState }

                payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
                payoutsOk = payoutConstraints payoutsByParty
                checkContinuation = case txOutContract of
                    Close -> traceIfFalse "L2" checkScriptOutputAny
                    _ -> let
                        totalIncome = foldMap (collectDeposits . getInputContent) inputs
                        totalPayouts = foldMap snd payoutsByParty
                        finalBalance = inputBalance + totalIncome - totalPayouts
                        in traceIfFalse "L1+" $ checkOwnOutputConstraint marloweData finalBalance
            preconditionsOk && inputsOk && payoutsOk && checkContinuation
        Error TEAmbiguousTimeIntervalError -> traceError "E1"
        Error TEApplyNoMatchError -> traceError "E2"
        Error (TEIntervalError (InvalidInterval _)) -> traceError "E3"
        Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "E4"
        Error TEUselessTransaction -> traceError "E5"
        Error TEHashMismatch -> traceError "E6"

  where
    MarloweParams{ rolesCurrency } = marloweParams

    findOwnInput :: ScriptContext -> Maybe TxInInfo
    findOwnInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Spending txOutRef} =
        find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
    findOwnInput _ = Nothing

    ownInput :: TxInInfo
    ownInput@TxInInfo{txInInfoResolved=TxOut{txOutAddress=ownAddress}} =
        case findOwnInput ctx of
            Just ownTxInInfo ->
                case filter (sameValidatorHash ownTxInInfo) (txInfoInputs scriptContextTxInfo) of
                    [i] -> i
                    _   -> traceError "I1" -- multiple Marlowe contract inputs with the same address, it's forbidden
            _ -> traceError "I0" {-"Can't find validation input"-}

    sameValidatorHash:: TxInInfo -> TxInInfo -> Bool
    sameValidatorHash
        TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential vh1) _}}
        TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential vh2) _}} = vh1 == vh2
    sameValidatorHash _ _ = False


    findDatumHash' :: PlutusTx.ToData o => o -> Maybe DatumHash
    findDatumHash' datum = findDatumHash (Datum $ PlutusTx.toBuiltinData datum) scriptContextTxInfo

    checkOwnOutputConstraint :: MarloweData -> Val.Value -> Bool
    checkOwnOutputConstraint ocDatum ocValue =
        let hsh = findDatumHash' ocDatum
        in traceIfFalse "L1" -- "Output constraint"
        $ checkScriptOutput ownAddress hsh ocValue getContinuingOutput

    getContinuingOutput :: TxOut
    getContinuingOutput = case filter (\TxOut{txOutAddress} -> ownAddress == txOutAddress) allOutputs of
        [out] -> out
        _     -> traceError "O0" -- no continuation or multiple Marlowe contract outputs, it's forbidden

    checkScriptOutput addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash svh} =
                    txOutValue == value && hsh == Just svh && txOutAddress == addr
    checkScriptOutput _ _ _ _ = False

    checkScriptOutputRelaxed addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash svh} =
                    txOutValue `Val.geq` value && hsh == Just svh && txOutAddress == addr
    checkScriptOutputRelaxed _ _ _ _ = False

    checkScriptOutputAny = all ((/= ownAddress) . txOutAddress) allOutputs

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
                hsh = findDatumHash' (rolesCurrency, role)
                addr = Address.scriptHashAddress rolePayoutValidatorHash
                in traceIfFalse "R" $ any (checkScriptOutputRelaxed addr hsh value) allOutputs

-- This is pretty standard way to minimize size of the typed validator:
--  * Wrap validator function so it accepts raw `BuiltinData`.
--  * Create a validator which is simply typed.
--  * Create "typed by `Any` validator".
--  * Coerce it if you like. This step is not required - we only need `TypedValidator`.
smallMarloweValidator :: Scripts.TypedValidator TypedMarloweValidator
smallMarloweValidator =
  let
    mkUntypedMarloweValidator :: ValidatorHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
    mkUntypedMarloweValidator rp = mkUntypedValidator (mkMarloweValidator rp)

    untypedValidator :: Scripts.Validator
    untypedValidator = mkValidatorScript $
      $$(PlutusTx.compile [|| mkUntypedMarloweValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode rolePayoutValidatorHash

    typedValidator :: Scripts.TypedValidator Scripts.Any
    typedValidator = Scripts.unsafeMkTypedValidator untypedValidator
  in
    unsafeCoerce typedValidator

marloweValidator :: Scripts.TypedValidator TypedMarloweValidator
marloweValidator = Scripts.mkTypedValidator
    @TypedMarloweValidator
    compiledMarloweValidator
    compiledArgsValidator
    where
        compiledMarloweValidator =
          $$(PlutusTx.compile [|| mkMarloweValidator ||])
            `PlutusTx.applyCode`
              PlutusTx.liftCode rolePayoutValidatorHash
        mkArgsValidator = mkUntypedValidator @MarloweData @MarloweInput
        compiledArgsValidator =
          $$(PlutusTx.compile [|| mkArgsValidator ||])

marloweValidatorHash :: ValidatorHash
marloweValidatorHash = Scripts.validatorHash marloweValidator

defaultTxValidationRange :: POSIXTime
defaultTxValidationRange = 10000

marloweTxInputFromInput :: Input -> MarloweTxInput
marloweTxInputFromInput (NormalInput i)         = Input i
marloweTxInputFromInput (MerkleizedInput i h _) = MerkleizedTxInput i h

marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = fmap marloweTxInputFromInput

makeLift ''MarloweTxInput
makeIsDataIndexed ''MarloweTxInput [('Input,0),('MerkleizedTxInput,1)]
