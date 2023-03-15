-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Generate random transactions for Plutus tests.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module Spec.Marlowe.Plutus.Transaction
  ( -- * Types
    ArbitraryTransaction
    -- * Generation
  , arbitraryPayoutTransaction
  , arbitrarySemanticsTransaction
    -- * Modification
  , merkleize
  , noModify
  , shuffle
    -- * Conditions
  , isScriptTxIn
  , noVeto
  ) where


import Control.Lens (Lens', use, uses, (.=), (<>=), (<~))
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT, lift)
import Data.Bifunctor (bimap, second)
import Data.List (nub, permutations)
import Language.Marlowe.Core.V1.Semantics
  ( MarloweData(MarloweData)
  , MarloweParams(..)
  , Payment(Payment)
  , TransactionInput(..)
  , TransactionOutput(..)
  , computeTransaction
  , paymentMoney
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( ChoiceId(ChoiceId)
  , Contract(Close)
  , Input(MerkleizedInput, NormalInput)
  , InputContent(IChoice, IDeposit)
  , Party(Role)
  , Payee(Party)
  , State(accounts)
  , Token(Token)
  , getInputContent
  )
import Language.Marlowe.Scripts (MarloweInput, MarloweTxInput(..))
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Value (gt)
import Plutus.V2.Ledger.Api
  ( Address(Address)
  , Credential(..)
  , CurrencySymbol
  , Datum(..)
  , DatumHash(..)
  , Extended(Finite, NegInf, PosInf)
  , Interval(Interval)
  , LowerBound(LowerBound)
  , OutputDatum(..)
  , PubKeyHash
  , Redeemer(..)
  , ScriptContext(..)
  , ScriptPurpose(Spending)
  , ToData(toBuiltinData)
  , TxInInfo(..)
  , TxInfo(..)
  , TxOut(TxOut, txOutAddress)
  , UpperBound(UpperBound)
  , Value
  )
import Spec.Marlowe.Plutus.Arbitrary ()
import Spec.Marlowe.Plutus.Lens ((<><~))
import Spec.Marlowe.Plutus.Script (payoutAddress, semanticsAddress)
import Spec.Marlowe.Plutus.Types
  ( PayoutTransaction(..)
  , PlutusTransaction(PlutusTransaction)
  , SemanticsTransaction(..)
  , amount
  , datum
  , infoData
  , infoFee
  , infoInputs
  , infoOutputs
  , infoSignatories
  , infoValidRange
  , input
  , inputContract
  , inputState
  , marloweParams
  , marloweParamsPayout
  , output
  , redeemer
  , role
  , scriptPurpose
  )
import Spec.Marlowe.Reference (ReferencePath, arbitraryReferenceTransaction)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryGoldenTransaction, arbitraryPositiveInteger)
import Spec.Marlowe.Semantics.Golden (GoldenTransaction)
import Spec.Marlowe.Semantics.Merkle (deepMerkleize, merkleizeInputs)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, elements, frequency, listOf, suchThat)

import qualified Language.Marlowe.Core.V1.Semantics.Types as M (Party(Address))
import qualified Plutus.V1.Ledger.Value as V (adaSymbol, adaToken, singleton)
import qualified PlutusTx.AssocMap as AM (fromList, toList)


-- | An arbitrary Plutus transaction.
type ArbitraryTransaction p a = StateT (PlutusTransaction p) Gen a


-- | Create a transaction generator for spending, with empty or default values.
bareSpending :: a -> Gen (PlutusTransaction a)
bareSpending p =
  PlutusTransaction p (Datum $ toBuiltinData ()) (Redeemer $ toBuiltinData ())
    <$> (
          ScriptContext
            <$> (TxInfo mempty mempty mempty mempty mempty mempty mempty (Interval (LowerBound NegInf False) (UpperBound PosInf True)) mempty mempty mempty <$> arbitrary)
            <*> (Spending <$> arbitrary)
        )


-- | Create a Marlowe semantics transaction, with mostly empty and default values.
bareSemanticsTransaction :: GoldenTransaction
                         -> Gen (PlutusTransaction SemanticsTransaction)
bareSemanticsTransaction (_state, _contract, _input, _output) =
  do
    rolesCurrency <- arbitrary
    let
      _params = MarloweParams{..}
    bareSpending SemanticsTransaction{..}


-- | Make the datum for a Marlowe semantics transaction.
makeSemanticsDatum :: MarloweParams
                   -> State
                   -> Contract
                   -> Datum
makeSemanticsDatum params state contract = Datum . toBuiltinData $ MarloweData params state contract


-- | Make the redeemer for a Marlowe semantics transaction.
makeSemanticsRedeemer :: MarloweInput
                      -> Redeemer
makeSemanticsRedeemer = Redeemer . toBuiltinData


-- | Convert Marlowe input to Marlowe transaction input.
inputToMarloweTxInput :: Input
                      -> (MarloweTxInput, [(DatumHash, Datum)])
inputToMarloweTxInput (NormalInput     content              ) = (Input             content     , []                                                )
inputToMarloweTxInput (MerkleizedInput content hash contract) = (MerkleizedTxInput content hash, [(DatumHash hash, Datum $ toBuiltinData contract)])


-- | Create script input for a Marlowe semantics transaction.
makeScriptInput :: ArbitraryTransaction SemanticsTransaction (TxInInfo, [(DatumHash, Datum)])
makeScriptInput =
  do
    inValue <- inputState `uses` totalValue
    inDatum <- use datum
    let inDatumHash = datumHash inDatum
    (, pure (inDatumHash, inDatum))
      . flip TxInInfo (TxOut semanticsAddress inValue (OutputDatumHash inDatumHash) Nothing)
      <$> lift arbitrary


-- | Total the value in a Marlowe state.
totalValue :: State -> Value
totalValue = foldMap (\((_, Token c n), i) -> V.singleton c n i) . AM.toList . accounts


-- | Create deposit inputs for a Marlowe semantics transaction.
makeDeposit :: Input
            -> ArbitraryTransaction SemanticsTransaction [TxInInfo]
makeDeposit input' =
  do
    ref <- lift arbitrary
    address' <- lift $ arbitrary `suchThat` notScriptAddress
    pure
      $ case getInputContent input' of
          IDeposit _ (M.Address _ address) (Token c n) i -> if i > 0
                                                              then pure . TxInInfo ref $ TxOut address  (V.singleton c n i) NoOutputDatum  Nothing
                                                              else mempty
          IDeposit _ (Role _             ) (Token c n) i -> if i > 0
                                                              then pure . TxInInfo ref $ TxOut address' (V.singleton c n i) NoOutputDatum  Nothing
                                                              else mempty
          _                                              -> mempty

-- | Create role input for a Marlowe semantics transaction.
makeRoleIn :: Input
           -> ArbitraryTransaction SemanticsTransaction [TxInInfo]
makeRoleIn input' =
  do
    MarloweParams currencySymbol <- use marloweParams
    ref <- lift arbitrary
    address  <- lift $ arbitrary `suchThat` notScriptAddress
    pure
      $ case getInputContent input' of
          IDeposit _ (Role role') _ _         -> pure . TxInInfo ref $ TxOut address (V.singleton currencySymbol role' 1) NoOutputDatum Nothing
          IChoice (ChoiceId _ (Role role')) _ -> pure . TxInInfo ref $ TxOut address (V.singleton currencySymbol role' 1) NoOutputDatum Nothing
          _                                   -> mempty


-- | Create script continuing output for a Marlowe semantics transaction.
makeScriptOutput :: ArbitraryTransaction SemanticsTransaction ([TxOut], [(DatumHash, Datum)])
makeScriptOutput =
  do
    params <- use marloweParams
    outState    <- output `uses` txOutState
    outContract <- output `uses` txOutContract
    let
      outDatum = Datum . toBuiltinData $ MarloweData params outState outContract
      outDatumHash = datumHash outDatum
    pure
      $ unzip
      [
        (
          TxOut semanticsAddress (totalValue outState) (OutputDatumHash outDatumHash) Nothing
        , (outDatumHash, outDatum)
        )
      |
        outContract /= Close
      ]


-- | Create role output for a Marlowe semantics transaction.
makeRoleOut :: TxInInfo
            -> ArbitraryTransaction SemanticsTransaction TxOut
makeRoleOut (TxInInfo _ (TxOut _ token _ _)) =
  TxOut <$> lift arbitrary <*> pure token <*> pure NoOutputDatum <*> pure Nothing


-- | Create a payment for a Marlowe semantics transaction.
makePayment :: CurrencySymbol
            -> Payment
            -> ArbitraryTransaction SemanticsTransaction([TxOut], [(DatumHash, Datum)])
makePayment _ payment@(Payment _ (Party (M.Address _ address)) _ _) =
  pure
    (
      pure $ TxOut address (paymentMoney payment) NoOutputDatum Nothing
    , mempty
    )
makePayment currencySymbol payment@(Payment _ (Party (Role role')) _ _) =
  do
    let
      roleDatum = Datum $ toBuiltinData (currencySymbol, role')
      roleDatumHash = datumHash roleDatum
    pure
      (
        pure $ TxOut payoutAddress (paymentMoney payment) (OutputDatumHash roleDatumHash) Nothing
      , pure (roleDatumHash, roleDatum)
      )
makePayment _ _ = pure (mempty, mempty)


-- | Create a deposit or choice signatory for a Marlowe semantics transaction.
makeActionSignatory :: Input
                    -> [PubKeyHash]
makeActionSignatory input' =
  case getInputContent input' of
    IDeposit _          (M.Address _ (Address (PubKeyCredential pkh) _))  _ _  -> pure pkh
    IChoice (ChoiceId _ (M.Address _ (Address (PubKeyCredential pkh) _))) _    -> pure pkh
    _                                                                          -> mempty


-- | Create a spending signatory for a Marlowe semantics transaction.
makeSpendSignatory :: TxInInfo
                   -> [PubKeyHash]
makeSpendSignatory (TxInInfo _ (TxOut (Address (PubKeyCredential pkh) _) _ _ _)) = pure pkh
makeSpendSignatory _                                                             = mempty


-- | Combine payments with the same address and hash.
consolidatePayments :: [TxOut] -> [TxOut]
consolidatePayments ps =
  let
    extractAddressHash (TxOut address _ hash _) = (address, hash)
    extractValue (TxOut _ value _ _) = value
  in
    [
      TxOut address value hash Nothing
    |
      ah@(address, hash) <- nub $ extractAddressHash <$> ps
    , let value = foldMap extractValue $ filter ((== ah) . extractAddressHash) ps
    ]


-- | Generate a valid Marlowe semantics transaction.
validSemanticsTransaction :: Bool                                          -- ^ Whether to add noise to the script context.
                          -> ArbitraryTransaction SemanticsTransaction ()  -- ^ The generator.
validSemanticsTransaction noisy =
  do
    -- The datum is `MarloweData`.
    datum <~ makeSemanticsDatum <$> use marloweParams <*> use inputState <*> use inputContract

    -- The redeemer is `MarloweInput`, but we also track the merkleizations.
    (marloweInput, merkleizations) <- input `uses` (second mconcat . unzip . fmap inputToMarloweTxInput . txInputs)
    redeemer .= makeSemanticsRedeemer marloweInput
    infoData <>= AM.fromList merkleizations

    -- Add the spending from the script.
    (inScript, inData) <- makeScriptInput
    infoInputs <>= [inScript]
    infoData <>= AM.fromList inData
    scriptPurpose .= Spending (txInInfoOutRef inScript)

    -- Add the role inputs.
    roleInputs <- fmap concat . mapM makeRoleIn . txInputs =<< use input
    infoInputs <>= roleInputs

    -- Add the deposits.
    infoInputs <><~ (fmap concat . mapM makeDeposit . txInputs =<< use input)

    -- Add the script output.
    (outScript, outData) <- makeScriptOutput
    infoOutputs <>= outScript
    infoData <>= AM.fromList outData

    -- Add the role outputs.
    infoOutputs <><~ mapM makeRoleOut roleInputs

    MarloweParams currencySymbol <- use marloweParams
    -- Add the payments.
    (payments, paymentData) <- fmap (bimap mconcat mconcat . unzip) . mapM (makePayment currencySymbol) =<< (output `uses` txOutPayments)
    infoOutputs <>= consolidatePayments payments
    infoData <>= AM.fromList paymentData

    -- Add the signatories.
    actionSignatories <- concatMap makeActionSignatory <$> input `uses` txInputs
    spendSignatories  <- concatMap makeSpendSignatory  <$> use infoInputs
    infoSignatories <>= nub (actionSignatories <> spendSignatories)

    -- Set the validity interval.
    interval <- input `uses` txInterval
    infoValidRange .= Interval (LowerBound (Finite $ fst interval) True) (UpperBound (Finite $ snd interval) True)

    -- Set the fee.
    infoFee <~ V.singleton V.adaSymbol V.adaToken <$> lift arbitraryPositiveInteger

    -- Add noise.
    when noisy addNoise

    -- Shuffle.
    shuffle


-- | Generate an arbitrary, valid Marlowe semantics transaction: datum, redeemer, and script context.
arbitrarySemanticsTransaction :: [ReferencePath]                               -- ^ The reference execution paths from which to choose.
                              -> ArbitraryTransaction SemanticsTransaction ()  -- ^ Modifications to make before building the valid transaction.
                              -> ArbitraryTransaction SemanticsTransaction ()  -- ^ Modifications to make after building the valid transaction.
                              -> Bool                                          -- ^ Whether to add noise to the script context.
                              -> Bool                                          -- ^ Whether to allow merkleization.
                              -> Gen (PlutusTransaction SemanticsTransaction)  -- ^ The generator.
arbitrarySemanticsTransaction referencePaths modifyBefore modifyAfter noisy allowMerkleization =
  do
    golden <-
      frequency
        [
          (1, arbitraryGoldenTransaction allowMerkleization)  -- Manually vetted transactions.
        , (5, arbitraryReferenceTransaction referencePaths)   -- Transactions generated using `getAllInputs` and `computeTransaction`.
        ]
    start <- bareSemanticsTransaction golden
    (modifyBefore >> validSemanticsTransaction noisy >> modifyAfter)
      `execStateT` start


-- | Create a Marlowe payout transaction, with mostly empty and default values.
barePayoutTransaction :: Gen (PlutusTransaction PayoutTransaction)
barePayoutTransaction =
  do
    rolesCurrency <- arbitrary
    let
      _params' = MarloweParams{..}
    _role <- arbitrary
    _amount <- arbitrary `suchThat` (`gt` mempty)
    bareSpending PayoutTransaction{..}


-- | Create a script input for a Marlowe payout transaction.
makePayoutIn :: ArbitraryTransaction PayoutTransaction (TxInInfo, (DatumHash, Datum))
makePayoutIn =
  do
    txInInfoOutRef <- lift arbitrary
    inDatum <- ((Datum . toBuiltinData) .) . (,) <$> marloweParamsPayout `uses` rolesCurrency <*> use role
    let
      inDatumHash = datumHash inDatum
    txInInfoResolved <- TxOut payoutAddress <$> use amount <*> pure (OutputDatumHash inDatumHash) <*> pure Nothing
    pure (TxInInfo{..}, (inDatumHash, inDatum))


-- | Create a role input for a Marlowe payout transaction.
makePayoutRoleIn :: ArbitraryTransaction PayoutTransaction TxInInfo
makePayoutRoleIn =
  do
    ref <- lift arbitrary
    address <- lift $ arbitrary `suchThat` notScriptAddress
    value <- V.singleton <$> marloweParamsPayout `uses` rolesCurrency <*> use role <*> pure 1
    pure
      . TxInInfo ref
      $ TxOut address value NoOutputDatum Nothing


-- | Create a payment output for a Marlowe payout transaction.
makePayoutOut :: ArbitraryTransaction PayoutTransaction TxOut
makePayoutOut =
  TxOut
    <$> lift arbitrary
    <*> use amount
    <*> pure NoOutputDatum
    <*> pure Nothing


-- | Create a role output for a Marlowe payout transaction.
makePayoutRoleOut :: ArbitraryTransaction PayoutTransaction TxOut
makePayoutRoleOut =
  do
    address <- lift arbitrary
    value <- V.singleton <$> marloweParamsPayout `uses` rolesCurrency <*> use role <*> pure 1
    pure
      $ TxOut address value NoOutputDatum Nothing


-- | Create a spending signatory for a Marlowe payout transaction.
makePayoutSignatory :: TxInInfo
                    -> [PubKeyHash]
makePayoutSignatory (TxInInfo _ (TxOut (Address (PubKeyCredential pkh) _ ) _ _ _)) = pure pkh
makePayoutSignatory _                                                              = mempty


-- | Generate a valid Marlowe payout transaction.
validPayoutTransaction :: Bool                                       -- ^ Whether to add noise to the script context.
                       -> ArbitraryTransaction PayoutTransaction ()  -- ^ The generator.
validPayoutTransaction noisy =
  do

    -- Add the script input.
    (inScript, inData@(_, inDatum)) <- makePayoutIn
    infoInputs <>= [inScript]
    infoData <>= AM.fromList [inData]
    scriptPurpose .= Spending (txInInfoOutRef inScript)

    -- The datum is the currency symbole and role name.
    datum .= inDatum

    -- The redeemer is unit.
    redeemer .= Redeemer (toBuiltinData ())

    -- Add the role input.
    infoInputs <><~ (pure <$> makePayoutRoleIn)

    -- Add the pay output.
    infoOutputs <><~ (pure <$> makePayoutOut)

    -- Add the role output.
    infoOutputs <><~ (pure <$> makePayoutRoleOut)

    -- Add the signatories.
    infoSignatories <><~ (infoInputs `uses` concatMap makePayoutSignatory)

    -- Set the fee.
    infoFee <~ V.singleton V.adaSymbol V.adaToken <$> lift arbitraryPositiveInteger

    -- Add noise.
    when noisy addNoise'

    -- Shuffle.
    shuffle


-- | Check that an address is not for a script.
notScriptAddress :: Address -> Bool
notScriptAddress (Address (ScriptCredential _) _) = False
notScriptAddress _ = True


-- | Check that an input is not from a script.
notScriptTxIn :: TxInInfo -> Bool
notScriptTxIn = not . isScriptTxIn


-- | Check that an input is from a script.
isScriptTxIn :: TxInInfo -> Bool
isScriptTxIn TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential _) _}} = True
isScriptTxIn _ = False


-- | Add noise to the inputs, outputs, and data in a Plutus transaction.
addNoise :: ArbitraryTransaction SemanticsTransaction ()
addNoise =
  do
    let
      isPayout (Payment _ (Party _) _ i) = i > 0
      isPayout _ = False
    hasPayments <- any isPayout . txOutPayments <$> use output
    let
      arbitraryInput =
        if hasPayments
          then arbitrary `suchThat` notScriptTxIn
          else arbitrary
    infoInputs  <><~ lift (listOf arbitraryInput `suchThat` ((< 5) . length))
    infoOutputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoData    <><~ lift (fmap AM.fromList $ arbitrary `suchThat` ((< 5) . length))


-- | Add noise to the inputs, outputs, and data in a Plutus transaction.
addNoise' :: ArbitraryTransaction PayoutTransaction ()
addNoise' =
  do
    infoInputs  <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoOutputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoData    <><~ lift (fmap AM.fromList $ arbitrary `suchThat` ((< 5) . length))


-- | Shuffle the order of inputs, outputs, data, and signatories in a Plutus transaction.
shuffle :: ArbitraryTransaction a ()
shuffle =
  do
    let
      go :: Lens' (PlutusTransaction a) [b] -> ArbitraryTransaction a ()
      go field = field <~ (lift . elements . permutations =<< use field)
    go infoInputs
    go infoOutputs
    go infoSignatories
    infoData <~ (lift . fmap AM.fromList . elements . permutations . AM.toList =<< use infoData)


-- | Merkleize a transaction.
merkleize :: ArbitraryTransaction SemanticsTransaction ()
merkleize =
  do
   -- Fetch the original state, contract, and inputs.
    state <- use inputState
    contract <- use inputContract
    inputs <- use input
    -- Merkleize the contract and the input.
    let
      (contract', continuations) = deepMerkleize contract
      inputs' = maybe (error "Merkleization of inputs failed.") id $ merkleizeInputs continuations state contract' inputs
    -- Update the contract, inputs, and outputs.
    inputContract .= contract'
    input .= inputs'
    output .= computeTransaction inputs' state contract'


-- | Generate an arbitrary, valid Marlowe payout transaction: datum, redeemer, and script context.
arbitraryPayoutTransaction :: ArbitraryTransaction PayoutTransaction ()  -- ^ Modifications to make before building the valid transaction.
                           -> ArbitraryTransaction PayoutTransaction ()  -- ^ Modifications to make after building the valid transaction.
                           -> Bool                                       -- ^ Whether to add noise to the script context.
                           -> Gen (PlutusTransaction PayoutTransaction)  -- ^ The generator.
arbitraryPayoutTransaction modifyBefore modifyAfter noisy =
  do
    start <- barePayoutTransaction
    (modifyBefore >> validPayoutTransaction noisy >> modifyAfter)
      `execStateT` start


-- | Do not modify a Plutus transaction.
noModify :: ArbitraryTransaction a ()
noModify = pure ()


-- | Do not eliminate generated Plutus transactions.
noVeto :: PlutusTransaction a -> Bool
noVeto = const True
