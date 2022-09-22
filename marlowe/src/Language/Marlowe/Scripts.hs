-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Marlowe validators.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Language.Marlowe.Scripts
  ( -- * Types
    MarloweInput
  , MarloweTxInput(..)
    -- * Semantics Validator
  , TypedMarloweValidator
  , alternateMarloweValidator
  , alternateMarloweValidatorHash
  , marloweValidator
  , marloweValidatorHash
    -- * Payout Validator
  , TypedRolePayoutValidator
  , rolePayoutValidator
  , rolePayoutValidatorHash
    -- * Utilities
  , marloweTxInputsFromInputs
  ) where


import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics as Semantics
import Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Pretty (Pretty(..))
import qualified Plutus.Script.Utils.Typed as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts (mkTypedValidator, mkUntypedValidator)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Address as Address (scriptHashAddress)
import qualified Plutus.V1.Ledger.Value as Val
import Plutus.V2.Ledger.Api
  ( Credential(..)
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
import qualified Plutus.V2.Ledger.Api as Ledger (Address(Address))
import Plutus.V2.Ledger.Contexts (findDatum, findDatumHash, txSignedBy, valueSpent)
import Plutus.V2.Ledger.Tx (OutputDatum(OutputDatumHash), TxOut(TxOut, txOutAddress, txOutDatum, txOutValue))
import PlutusTx (makeIsDataIndexed, makeLift)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Plugin ()
import PlutusTx.Prelude as PlutusTxPrelude
import qualified Prelude as Haskell
import Unsafe.Coerce (unsafeCoerce)


-- | Input to a Marlowe transaction.
type MarloweInput = [MarloweTxInput]


-- | Tag for the Marlowe semantics validator.
data TypedMarloweValidator


-- Datum and redeemer types for the Marlowe semantics validator.
-- [Marlowe-Cardano Specification: "Constraint 1. Typed validation".]
instance Scripts.ValidatorTypes TypedMarloweValidator where
    type instance RedeemerType TypedMarloweValidator = MarloweInput
    type instance DatumType TypedMarloweValidator = MarloweData


-- | Tag for the Marlowe payout validator.
data TypedRolePayoutValidator


-- Datum and redeemer types for the Marlowe payout validator.
-- [Marlowe-Cardano Specification: "Constraint 16. Typed validation".]
instance Scripts.ValidatorTypes TypedRolePayoutValidator where
  type instance RedeemerType TypedRolePayoutValidator = ()
  type instance DatumType TypedRolePayoutValidator = (CurrencySymbol, TokenName)


-- | A single input applied in the Marlowe semantics validator.
data MarloweTxInput = Input InputContent
                    | MerkleizedTxInput InputContent BuiltinByteString
  deriving stock (Haskell.Show,Haskell.Eq,Generic)
  deriving anyclass (Pretty)


-- | The Marlowe payout validator.
rolePayoutValidator :: Scripts.TypedValidator TypedRolePayoutValidator
rolePayoutValidator = mkTypedValidator @TypedRolePayoutValidator
  $$(PlutusTx.compile [|| mkRolePayoutValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @(CurrencySymbol, TokenName) @()


{-# INLINABLE rolePayoutValidator #-}
-- | The Marlowe payout validator.
mkRolePayoutValidator :: (CurrencySymbol, TokenName)  -- ^ The datum is the currency symbol and role name for the payout.
                      -> ()                           -- ^ No redeemer is required.
                      -> ScriptContext                -- ^ The script context.
                      -> Bool                         -- ^ Whether the transaction validated.
mkRolePayoutValidator (currency, role) _ ctx =
    -- The role token for the correct currency must be present.
    -- [Marlowe-Cardano Specification: "16. Payment authorized".]
    Val.singleton currency role 1 `Val.leq` valueSpent (scriptContextTxInfo ctx)


-- | The hash of the Marlowe payout validator.
rolePayoutValidatorHash :: ValidatorHash
rolePayoutValidatorHash = Scripts.validatorHash rolePayoutValidator


{-# INLINABLE closeInterval #-}
-- | Convert a Plutus POSIX time range into the closed interval needed by Marlowe semantics.
closeInterval :: POSIXTimeRange -> Maybe (POSIXTime, POSIXTime)
closeInterval (Interval (LowerBound (Finite (POSIXTime l)) lc) (UpperBound (Finite (POSIXTime h)) hc)) =
  Just
    (
      POSIXTime $ l + 1 - fromEnum lc  -- Add one millisecond if the interval was open.
    , POSIXTime $ h - 1 + fromEnum hc  -- Subtract one millisecond if the interval was open.
    )
closeInterval _ = Nothing


{-# INLINABLE mkMarloweValidator #-}
-- | The Marlowe semantics validator.
mkMarloweValidator
    :: ValidatorHash  -- ^ The hash of the corresponding Marlowe payout validator.
    -> MarloweData    -- ^ The datum is the Marlowe parameters, state, and contract.
    -> MarloweInput   -- ^ The redeemer is the list of inputs applied to the contract.
    -> ScriptContext  -- ^ The script context.
    -> Bool           -- ^ Whether the transaction validated.
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
                Nothing        -> traceError "a"
    -- The incoming balance of each account must be positive.
    -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
    let positiveBalances = traceIfFalse "b" $ validateBalances marloweState

    -- Find Contract continuation in TxInfo datums by hash or fail with error.
    let inputs = fmap marloweTxInputToInput marloweTxInputs
    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.  This gives huge flexibility by allowing
        parties to provide multiple inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputContents = fmap getInputContent inputs
    -- Check that the required signatures and role tokens are present.
    -- [Marlowe-Cardano Specification: "Constraint 14. Inputs authorized".]
    let inputsOk = validateInputs inputContents

    -- Since individual balances were validated to be positive,
    -- the total balance is also positive.
    let inputBalance = totalBalance (accounts marloweState)

    -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
    -- The total incoming balance must match the actual script value being spent.
    let balancesOk = traceIfFalse "v" $ inputBalance == scriptInValue

    let preconditionsOk = positiveBalances && balancesOk

    -- Package the inputs to be applied in the semantics.
    -- [Marlowe-Cardano Specification: "Constraint 0. Input to semantics".]
    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    -- [Marlowe-Cardano Specification: "Constraint 7. Input state".]
    -- [Marlowe-Cardano Specification: "Constraint 8. Input contract".]
    -- The semantics computation operates on the state and contract from
    -- the incoming datum.
    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            -- [Marlowe-Cardano Specification: "Constraint 9. Marlowe parameters".]
            -- [Marlowe-Cardano Specification: "Constraint 10. Output state".]
            -- [Marlowe-Cardano Specification: "Constraint 11. Output contract."]
            -- The output datum maintains the parameters and uses the state
            -- and contract resulting from the semantics computation.
            let marloweData = MarloweData {
                    marloweParams = marloweParams,
                    marloweContract = txOutContract,
                    marloweState = txOutState }

                -- Each party must receive as least as much value as the semantics specify.
                -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient payment."]
                payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
                payoutsOk = payoutConstraints payoutsByParty

                checkContinuation = case txOutContract of
                    -- [Marlowe-Cardano Specification: "Constraint 4. No output to script on close".]
                    Close -> traceIfFalse "c" checkScriptOutputAny
                    _ -> let
                        totalIncome = foldMap collectDeposits inputContents
                        totalPayouts = foldMap snd payoutsByParty
                        finalBalance = inputBalance + totalIncome - totalPayouts
                        -- The total account balance must be paid to a single output to the script.
                        -- [Marlowe-Cardano Specification: "Constraint 3. Single Marlowe output".]
                        -- [Marlowe-Cardano Specification: "Constraint 6. Output value to script."]
                        in checkOwnOutputConstraint marloweData finalBalance
            preconditionsOk && inputsOk && payoutsOk && checkContinuation
        Error TEAmbiguousTimeIntervalError -> traceError "i"
        Error TEApplyNoMatchError -> traceError "n"
        Error (TEIntervalError (InvalidInterval _)) -> traceError "j"
        Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "k"
        Error TEUselessTransaction -> traceError "u"
        Error TEHashMismatch -> traceError "m"

  where

    -- The roles currency is in the Marlowe parameters.
    MarloweParams{ rolesCurrency } = marloweParams

    -- Find the input being spent by a script.
    findOwnInput :: ScriptContext -> Maybe TxInInfo
    findOwnInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Spending txOutRef} =
        find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
    findOwnInput _ = Nothing

    -- The inputs being spent by this script.
    -- [Marlowe-Cardano Specification: "2. Single Marlowe script input".]
    ownInput :: TxInInfo
    ownInput@TxInInfo{txInInfoResolved=TxOut{txOutAddress=ownAddress}} =
        case findOwnInput ctx of
            Just ownTxInInfo ->
                case filter (sameValidatorHash ownTxInInfo) (txInfoInputs scriptContextTxInfo) of
                    [i] -> i
                    _   -> traceError "w" -- Multiple Marlowe contract inputs with the same address are forbidden.
            _ -> traceError "x" -- Input to be validated was not found.

    -- Check if inputs are being spent from the same script.
    sameValidatorHash:: TxInInfo -> TxInInfo -> Bool
    sameValidatorHash
        TxInInfo{txInInfoResolved=TxOut{txOutAddress=Ledger.Address (ScriptCredential vh1) _}}
        TxInInfo{txInInfoResolved=TxOut{txOutAddress=Ledger.Address (ScriptCredential vh2) _}} = vh1 == vh2
    sameValidatorHash _ _ = False

    -- Look up the Datum hash for specific data.
    findDatumHash' :: PlutusTx.ToData o => o -> Maybe DatumHash
    findDatumHash' datum = findDatumHash (Datum $ PlutusTx.toBuiltinData datum) scriptContextTxInfo

    -- Check that the correct datum and value is being output to the script.
    checkOwnOutputConstraint :: MarloweData -> Val.Value -> Bool
    checkOwnOutputConstraint ocDatum ocValue =
        let hsh = findDatumHash' ocDatum
        in traceIfFalse "d" -- "Output constraint"
        $ checkScriptOutput ownAddress hsh ocValue getContinuingOutput

    getContinuingOutput :: TxOut
    getContinuingOutput = case filter (\TxOut{txOutAddress} -> ownAddress == txOutAddress) allOutputs of
        [out] -> out
        _     -> traceError "o" -- No continuation or multiple Marlowe contract outputs is forbidden.

    -- Check that address, value, and datum match the specified.
    checkScriptOutput :: Ledger.Address -> Maybe DatumHash -> Val.Value -> TxOut -> Bool
    checkScriptOutput addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash svh} =
                    txOutValue == value && hsh == Just svh && txOutAddress == addr
    checkScriptOutput _ _ _ _ = False

    -- Check that address and datum match the specified, and that value is at least that required.
    checkScriptOutputRelaxed :: Ledger.Address -> Maybe DatumHash -> Val.Value -> TxOut -> Bool
    checkScriptOutputRelaxed addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash svh} =
                    txOutValue `Val.geq` value && hsh == Just svh && txOutAddress == addr
    checkScriptOutputRelaxed _ _ _ _ = False

    -- Check for any output to the script address.
    checkScriptOutputAny :: Bool
    checkScriptOutputAny = all ((/= ownAddress) . txOutAddress) allOutputs

    -- All of the script outputs.
    allOutputs :: [TxOut]
    allOutputs = txInfoOutputs scriptContextTxInfo

    -- Check mekleization and transform transaction input to semantics input.
    marloweTxInputToInput :: MarloweTxInput -> Input
    marloweTxInputToInput (MerkleizedTxInput input hash) =
        case findDatum (DatumHash hash) scriptContextTxInfo of
            Just (Datum d) -> let
                continuation = PlutusTx.unsafeFromBuiltinData d
                in MerkleizedInput input hash continuation
            Nothing -> traceError "h"
    marloweTxInputToInput (Input input) = NormalInput input

    -- Check that inputs are authorized.
    validateInputs :: [InputContent] -> Bool
    validateInputs = all validateInputWitness
      where
        validateInputWitness :: InputContent -> Bool
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party  -- The party must witness a deposit.
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party  -- The party must witness a choice.
                INotify                      -> True                        -- No witness is needed for a notify.
          where
            validatePartyWitness :: Party -> Bool
            validatePartyWitness (Address _ address) = traceIfFalse "s" $ txSignedByAddress address  -- The key must have signed.
            validatePartyWitness (Role role)         = traceIfFalse "t"                              -- The role token must be present.
                                                       $ Val.singleton rolesCurrency role 1 `Val.leq` valueSpent scriptContextTxInfo

    -- Tally the deposits in the input.
    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = zero

    -- Extract the payout to a party.
    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) (Token cur tok) amount)
      | amount > 0 = AssocMap.singleton party $ Val.singleton cur tok amount
      | otherwise  = AssocMap.empty  -- NOTE: Perhaps required because semantics may make zero payments
                                     -- (though this passes the test suite), but removing this function's
                                     -- guard reduces the validator size by 20 bytes.
    payoutByParty (Payment _ (Account _) _ _ )       = AssocMap.empty

    -- Check outgoing payments.
    payoutConstraints :: [(Party, Val.Value)] -> Bool
    payoutConstraints payoutsByParty = all payoutToTxOut payoutsByParty
      where
        payoutToTxOut :: (Party, Val.Value) -> Bool
        payoutToTxOut (party, value) = case party of
            Address _ address  -> traceIfFalse "p" $ value `Val.leq` valuePaidToAddress address  -- At least sufficient value paid.
            Role role -> let
                hsh = findDatumHash' (rolesCurrency, role)
                addr = Address.scriptHashAddress rolePayoutValidatorHash
                -- Some output must have the correct value and datum to the role-payout address.
                in traceIfFalse "r" $ any (checkScriptOutputRelaxed addr hsh value) allOutputs

    -- The key for the address must have signed.
    txSignedByAddress :: Ledger.Address -> Bool
    txSignedByAddress (Ledger.Address (PubKeyCredential pkh) _) = scriptContextTxInfo `txSignedBy` pkh
    txSignedByAddress _                                         = False

    -- Tally the value paid to an address.
    valuePaidToAddress :: Ledger.Address -> Val.Value
    valuePaidToAddress address = foldMap txOutValue $ filter ((== address) . txOutAddress) allOutputs


-- | The validator for Marlowe semantics.
--
-- This is pretty standard way to minimize size of the typed validator:
--  * Wrap validator function so it accepts raw `BuiltinData`.
--  * Create a validator which is simply typed.
--  * Create "typed by `Any` validator".
--  * Coerce it if you like. This step is not required - we only need `TypedValidator`.
marloweValidator :: Scripts.TypedValidator TypedMarloweValidator
marloweValidator =
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

-- | The hash of the Marlowe semantics validator.
marloweValidatorHash :: ValidatorHash
marloweValidatorHash = Scripts.validatorHash marloweValidator


{-# DEPRECATED alternateMarloweValidator "This validator is too large. Use `marloweValidator` instead." #-}
-- | An alternative version of the Marlowe semantics validator that does uses straightforward validator
-- typing, but at the expense of a larger size.
alternateMarloweValidator :: Scripts.TypedValidator TypedMarloweValidator
alternateMarloweValidator = Scripts.mkTypedValidator
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


{-# DEPRECATED alternateMarloweValidatorHash "This validator is too large. Use `marloweValidatorHash` instead." #-}
-- | Hash of the alaternative Marlowe semantics validator.
alternateMarloweValidatorHash :: ValidatorHash
alternateMarloweValidatorHash = Scripts.validatorHash alternateMarloweValidator


-- | Convert semantics input to transaction input.
marloweTxInputFromInput :: Input -> MarloweTxInput
marloweTxInputFromInput (NormalInput i)         = Input i
marloweTxInputFromInput (MerkleizedInput i h _) = MerkleizedTxInput i h


-- | Convert semantics inputs to transaction inputs.
marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = fmap marloweTxInputFromInput


-- Lifting data types to Plutus Core
makeLift ''MarloweTxInput
makeIsDataIndexed ''MarloweTxInput [('Input,0),('MerkleizedTxInput,1)]
