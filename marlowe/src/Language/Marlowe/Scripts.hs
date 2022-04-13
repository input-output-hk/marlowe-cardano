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
-- {-# OPTIONS_GHC -fno-omit-interface-pragmas #-}


module Language.Marlowe.Scripts where
import GHC.Generics
import Language.Marlowe.Pretty (Pretty (..))
import Language.Marlowe.Semantics
import Language.Marlowe.SemanticsTypes
import Ledger
import Ledger.Ada (adaSymbol)
import qualified Ledger.Contexts as V
import qualified Ledger.Interval as Interval
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Val
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Scripts as Scripts
import PlutusTx (makeIsDataIndexed, makeLift)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude
import qualified Prelude as Haskell
import Unsafe.Coerce
import qualified UntypedPlutusCore as UPLC

type MarloweTimeRange = (POSIXTime, POSIXTime)
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


rolePayoutScript :: Validator
rolePayoutScript = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]))
  where
    wrapped = Scripts.wrapValidator rolePayoutValidator


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: (CurrencySymbol, TokenName) -> () -> ScriptContext -> Bool
rolePayoutValidator (currency, role) _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role > 0


mkRolePayoutValidatorHash :: ValidatorHash
mkRolePayoutValidatorHash = validatorHash rolePayoutScript


defaultRolePayoutValidatorHash :: ValidatorHash
defaultRolePayoutValidatorHash = mkRolePayoutValidatorHash


-- {-# INLINABLE smallMarloweValidator #-}

{-
    Off-chain
    Contract -> [role]
    Contract -> [role] -> [(role, amount)]
    () -> TxInRef
    TxInRef -> rolePayoutValidatorHash (unique, not enforced)
    [(role, amount)], txInRef, rolePayoutValidatorHash -> MarloweParams
    [(role, amount)], txInRef, rolePayoutValidatorHash, initial MarloweDataHash -> UniversalHash (MPSHash == ValidatorHash == CurrencySymbol)
    MPS checks during minting, either:
    - role tokens minted && a single TxOut exists with same hash validator, and DatumHash == initial MarloweDataHash
    - role tokens can be destroyed
    Validator checks
    - single input-output with same TxInRef


 -}
mkRolesCurrency params = let
    ValidatorHash hash = Scripts.validatorHash $ universalMarloweValidator params
    in Val.CurrencySymbol hash

{-# INLINABLE smallMarloweValidator #-}
smallMarloweValidator
    :: MarloweParams
    -> (BuiltinData -> Contract)
    -> (BuiltinData -> ScriptContext)
    -> MarloweData
    -> MarloweInput
    -> BuiltinData
    -> ()
smallMarloweValidator MarloweParams{rolePayoutValidatorHash}
    fromDataToContract
    fromDataToScriptContext
    MarloweData{..}
    marloweTxInputs
    builtInDataCtx = do

    let ctx@ScriptContext{scriptContextTxInfo} = fromDataToScriptContext builtInDataCtx

    let findOwnInput :: ScriptContext -> Maybe TxInInfo
        findOwnInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Spending txOutRef} =
            find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
        findOwnInput _ = Nothing

        ownInput :: TxInInfo
        ownInput@TxInInfo{txInInfoResolved=TxOut{txOutAddress=ownAddress@(Address (ScriptCredential (ValidatorHash ownValidatorHash)) _)}} =
            case findOwnInput ctx of
                Just ownTxInInfo ->
                    case filter (sameValidatorHash ownTxInInfo) (txInfoInputs scriptContextTxInfo) of
                        [i] -> i
                        _   -> traceError "I1" -- multiple Marlowe contract inputs with the same address, it's forbidden
                _ -> traceError "I0" {-"Can't find validation input"-}

        rolesCurrency = Val.CurrencySymbol ownValidatorHash

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

        checkScriptOutput addr hsh value TxOut{txOutAddress, txOutValue, txOutDatumHash=Just svh} =
                        txOutValue == value && hsh == Just svh && txOutAddress == addr
        checkScriptOutput _ _ _ _ = False

        allOutputs :: [TxOut]
        allOutputs = txInfoOutputs scriptContextTxInfo

        marloweTxInputToInput :: MarloweTxInput -> Input
        marloweTxInputToInput (MerkleizedTxInput input hash) =
            case findDatum (DatumHash hash) scriptContextTxInfo of
                Just (Datum d) -> let
                    -- continuation = PlutusTx.unsafeFromBuiltinData d
                    continuation = fromDataToContract d
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
                        addr = Ledger.scriptHashAddress rolePayoutValidatorHash
                        in traceIfFalse "R" $ any (checkScriptOutput addr hsh value) allOutputs

    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let interval =
            case txInfoValidRange scriptContextTxInfo of
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
                        in checkOwnOutputConstraint marloweData finalBalance
            if preconditionsOk && inputsOk && payoutsOk && checkContinuation
            then () else traceError "M"

        Error TEAmbiguousTimeIntervalError -> traceError "E1"
        Error TEApplyNoMatchError -> traceError "E2"
        Error (TEIntervalError (InvalidInterval _)) -> traceError "E3"
        Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "E4"
        Error TEUselessTransaction -> traceError "E5"
        Error TEHashMismatch -> traceError "E6"


smallUntypedValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
smallUntypedValidator params = let
    -- wrapped s = Scripts.wrapValidator (smallMarloweValidator s)
    wrapped s = wrapper (smallMarloweValidator s)
    typed = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    -- Yeah, I know. It works, though.
    -- Remove this when Typed Validator has the same size as untyped.
    in unsafeCoerce (Scripts.unsafeMkTypedValidator typed)


smallUntypedValidatorScript :: MarloweParams -> Scripts.Script
smallUntypedValidatorScript =  getValidator . Scripts.validatorScript . smallUntypedValidator


applyScript :: Script -> Script -> Script
applyScript (Script f) (Script arg) = Script $ UPLC.applyProgram f arg


{-# INLINABLE marloweMonetaryPolicy #-}
marloweMonetaryPolicy :: MarloweParams -> AssocMap.Map TokenName Integer -> ScriptContext -> ()
marloweMonetaryPolicy MarloweParams{uniqueTxOutRef=(refHash, refIdx)} tokens ctx@ScriptContext{scriptContextTxInfo=txinfo} = let
    -- see note [Obtaining the currency symbol]
    ownSymbol@Val.CurrencySymbol{unCurrencySymbol=ownHash} = ownCurrencySymbol ctx
    ownValidatorHash = ValidatorHash ownHash

    minted = V.txInfoMint txinfo
    expected = Val.Value $ AssocMap.singleton ownSymbol tokens

    -- True if the pending transaction mints the amount of
    -- currency that we expect
    mintOK =
        let v = expected == minted
        in traceIfFalse "C0" {-"Value minted different from expected"-} v

    -- True if the pending transaction spends the output
    -- identified by @(refHash, refIdx)@
    txOutputSpent =
        let v = V.spendsOutput txinfo refHash refIdx
        in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    marloweTxOutExists TxOut{txOutAddress=Address (ScriptCredential vh) _} | vh == ownValidatorHash = True
    marloweTxOutExists _ = False

    -- Ensure that a TxOut exists with ValidatorHash same as this MintingPolicyHash
    marloweContractTxOutExists = any marloweTxOutExists $ txInfoOutputs txinfo

    in if mintOK && txOutputSpent && marloweContractTxOutExists then () else traceError "E"


{-- * Note [Universal Script]

    How to make a single script that works as both a MintingPolicy and as a Validator.

    A MintingPolicy script is a function of 2 arguments, and has a signature

    mintingPolicy :: BuiltinData -> BuiltinData -> ()
    mintingPolicy redeemer context = ...

    and a Validator is a function of 3 arguments, and has a signature

    validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    validator redeemer datum context = ...

    We can distinguish the context in which a script is called by
    checking whether the second argument is 'ScriptContext' or not.
    We can do this by trying do deserialize the second argument into ScriptContext
    using 'fromBuiltinData'. If it deserializes successfully we are called as a Minting Policy,
    otherwise it's a Validator context.

    To be run in both contexts, the universal script must expect 2 arguments,
    'Redeemer' and 'ScriptContext' for of MintingPolicy, and 'Redeemer' and 'Datum' for a Validator.
    In case of Minting Policy it should return () or error.
    But as a Validator, the script should return a continuation:
    (\ctx :: ScriptContext -> validator logic)

    This obviously does not typecheck as Haskell can't unify () and ScriptContext -> () types.
    Moreover, PlutusTx does not support usage of 'unsafeCoerce'.

    In order to convince the compiler we are going to exploit the fact
    that Plutus script is actually an untyped lambda calculus.

    We parameterize our 'universalScript' with a validator function 'f' that has a signature

    f :: BuiltinData -> BuiltinData -> ()

    universalPlutusCode :: (BuiltinData -> BuiltinData -> ()) -> BuiltinData -> BuiltinData -> ()
    universalPlutusCode f a b = if isScriptContext b then mintingPolicy a b else f a b

    While our validator code would look like that:

    validatorPlutusCode :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    validatorPlutusCode redeemer datum context = ...

    Compile thes function to Plutus Script:

    universalScript = Scripts.fromCompiledCode $$(PlutusTx.compile [|| universalPlutusCode ||]
    validatorScript = Scripts.fromCompiledCode $$(PlutusTx.compile [|| validatorPlutusCode ||]

    Here is the trick: we apply universalScript to validatorScript as an argument.

    Then, the 'validatorPlutusCode' (binded as 'f' in our 'universalScript')
    applied to 2 arguments will return a continuation(\ctx -> ...).
    And that's precisely what we wanted!

    There is a significant drawback with this approach, though.
    As we compile these 2 scripts separately, the common Plutus code gets generated and included twice,
    because Haskell compiler can't optimize/reuse it.
    The large amount of code is generated by 'unsafeFromBuiltinData' function, almost 4k bytes.
    Simplest solution is to pass instantiations of the function as arguments into 'validatorPlutusCode'
    and reuse it manually.

    In the future we expect the serialization to become a builtin, so it won't be a size issue.

    Another possibility could be adding support for usafeCoerce to PlutusTx compiler.
    Then we could just directly 'unsafeCoerse (f a b) :: ()' in the 'universalScript'.
-}

universalMarlowePlutusCode :: MarloweParams -> ((BuiltinData -> Contract) -> (BuiltinData -> ScriptContext) -> MarloweData -> MarloweInput -> ()) -> BuiltinData -> BuiltinData -> ()
universalMarlowePlutusCode mp f a b = let
    -- if script's second argument is ScriptContext then it's a MintingPolicy
    -- otherwise, it's a Validator
    mctx :: Maybe ScriptContext
    mctx = PlutusTx.fromBuiltinData b
    in case mctx of
        Just ctx -> marloweMonetaryPolicy mp (PlutusTx.unsafeFromBuiltinData a) ctx
        -- this call should return a continuation (\scriptContext -> validator logic)
        _        -> f PlutusTx.unsafeFromBuiltinData PlutusTx.unsafeFromBuiltinData
                        (PlutusTx.unsafeFromBuiltinData a) (PlutusTx.unsafeFromBuiltinData b)


{-# INLINABLE wrapper #-}
wrapper :: ((BuiltinData -> Contract) -> (BuiltinData -> ScriptContext) -> MarloweData -> MarloweInput -> BuiltinData -> ()) -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper f a b c = f PlutusTx.unsafeFromBuiltinData PlutusTx.unsafeFromBuiltinData (PlutusTx.unsafeFromBuiltinData a) (PlutusTx.unsafeFromBuiltinData b) c


marloweMPS :: MarloweParams -> Scripts.Script
marloweMPS params = Scripts.fromCompiledCode
    ($$(PlutusTx.compile [|| universalMarlowePlutusCode ||])
        `PlutusTx.applyCode` PlutusTx.liftCode params)


universalMarloweScript :: MarloweParams -> Scripts.Script
universalMarloweScript params = applyScript mps validator
  where
    validator = smallUntypedValidatorScript params
    mps = marloweMPS params


universalMarloweValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
universalMarloweValidator params = unsafeCoerce (Scripts.unsafeMkTypedValidator validator)
 where
   validator = Validator $ universalMarloweScript params


universalMarloweMintingPolicy :: MarloweParams -> MintingPolicy
universalMarloweMintingPolicy params = MintingPolicy $ universalMarloweScript params


defaultTxValidationRange :: POSIXTime
defaultTxValidationRange = 10000

marloweTxInputFromInput :: Input -> MarloweTxInput
marloweTxInputFromInput (NormalInput i)         = Input i
marloweTxInputFromInput (MerkleizedInput i h _) = MerkleizedTxInput i h

marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = fmap marloweTxInputFromInput

makeLift ''MarloweTxInput
makeIsDataIndexed ''MarloweTxInput [('Input,0),('MerkleizedTxInput,1)]
