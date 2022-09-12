-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of Marlowe's Plutus implementation against its on-chain specification.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Spec.Marlowe.Plutus.Specification (
-- * Testing
  tests
) where


import Control.Lens (use, uses, (%=), (<>=), (^.))
import Control.Monad.State (lift)
import Data.Bifunctor (bimap)
import Data.Proxy (Proxy (..))
import Data.These (These (That, These, This))
import Language.Marlowe.Core.V1.Semantics (MarloweData (..), MarloweParams (rolesCurrency), Payment (Payment),
                                           TransactionInput (..),
                                           TransactionOutput (txOutContract, txOutPayments, txOutState))
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId (ChoiceId), Contract (Close), Input (..),
                                                 InputContent (IChoice, IDeposit), Party (PK, Role), Payee (Party),
                                                 State (accounts))
import Language.Marlowe.Scripts (MarloweInput)
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Api (Address (Address), BuiltinData (BuiltinData), Credential (PubKeyCredential),
                             Data (B, Constr, List), Datum (Datum), FromData (..), PubKeyHash, ScriptContext,
                             ToData (..), TokenName, TxInInfo (TxInInfo, txInInfoResolved), TxOut (TxOut, txOutValue),
                             Value, adaSymbol, adaToken, singleton, toData)
import Plutus.V1.Ledger.Value (flattenValue, gt, valueOf)
import Spec.Marlowe.Plutus.Script (evaluatePayout, evaluateSemantics, payoutAddress, semanticsAddress)
import Spec.Marlowe.Plutus.Transaction (ArbitraryTransaction, arbitraryPayoutTransaction, arbitrarySemanticsTransaction,
                                        noModify, noVeto, shuffle)
import Spec.Marlowe.Plutus.Types (PayoutTransaction (_params'), PlutusTransaction (..), SemanticsTransaction (_params),
                                  infoData, infoInputs, infoOutputs, infoSignatories, input, inputState, marloweParams,
                                  marloweParamsPayout, output, role)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, forAll, property, suchThat, testProperty)

import qualified PlutusTx.AssocMap as AM (insert)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Marlowe On-Chain Specification"
    [
      testGroup "Semantics Validator"
        [
          testGroup "Valid transaction succeeds"
            [
              testProperty "Noiseless" $ checkSemanticsTransaction noModify noModify noVeto True False
            , testProperty "Noisy"     $ checkSemanticsTransaction noModify noModify noVeto True True
            ]
        , testGroup "Constraint 1. Typed validation"
            [
              testProperty "Valid datum deserializes"                    $ check1Valid   (arbitrary :: Gen MarloweData  )
            , testProperty "Valid redeemer deserializes"                 $ check1Valid   (arbitrary :: Gen MarloweInput )
            , testProperty "Valid script context deserializes"           $ check1Valid   (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize"          $ check1Invalid (Proxy :: Proxy MarloweData  ) True  True True
            , testProperty "Invalid redeemer does not deserialize"       $ check1Invalid (Proxy :: Proxy MarloweInput ) False True True
            , testProperty "Invalid script context does not deserialize" $ check1Invalid (Proxy :: Proxy ScriptContext) True  True True
            ]
        , testGroup "Constraint 2. Single Marlowe script input"
            [
              testProperty "Invalid attempt to run two Marlowe scripts" checkDoubleInput
            ]
        , testGroup "Constraint 3. Single Marlowe output"
            [
              testProperty "Invalid attempt to split Marlowe output" checkMultipleOutput
            ]
        , testGroup "FAILURE OF Constraint 4. No output to script on close"
            [
              testProperty "Invalid attempt to output to Marlowe on close" checkCloseOutput
            ]
        , testGroup "Constraint 5. Input value from script"
            [
              testProperty "Invalid mismatch between state and script input" checkValueInput
            ]
        , testGroup "Constraint 6. Output value to script"
            [
              testProperty "Invalid mismatch between state and script input" checkValueOutput
            ]
        , testGroup "Constraint 7. Input state"
            [
              -- TODO: This test requires instrumenting the Plutus script.
            ]
        , testGroup "Constraint 8. Input contract"
            [
              -- TODO: This test requires instrumenting the Plutus script.
            ]
        , testGroup "Constraint 9. Marlowe parameters"
            [
              -- TODO: This test requires instrumenting the Plutus script.
            ]
        , testGroup "Constraint 10. Output state"
            [
              testProperty "Invalid mismatch between state and script output" checkStateOutput
            ]
        , testGroup "Constraint 11. Output contract"
            [
              testProperty "Invalid mismatch between contract and script output" checkContractOutput
            ]
        , testGroup "Constraint 12. Merkleized continuations"
            [
              -- FIXME: Add generator for merkleized contracts.
            ]
        , testGroup "Constraint 13. Positive balances"
            [
              testProperty "Invalid non-positive balance" checkPositiveAccounts
            ]
        , testGroup "Constraint 14. Inputs authorized"
            [
              testProperty "Invalid missing authorization" checkAuthorization
            ]
        , testGroup "Constraint 15. Sufficient payment"
            [
              testProperty "Invalid insufficient payment" checkPayment
            ]
        ]
    , testGroup "Payout Validator"
        [
          testGroup "Valid transaction succeeds"
            [
              testProperty "Noiseless" $ checkPayoutTransaction noModify noModify noVeto True False
            , testProperty "Noisy"     $ checkPayoutTransaction noModify noModify noVeto True True
            ]
        , testGroup "Constraint 16. Typed validation"
            [
              testProperty "Valid datum deserializes"                    $ check1Valid   (arbitrary :: Gen TokenName    )
            , testProperty "Valid redeemer deserializes"                 $ check1Valid   (arbitrary :: Gen ()           )
            , testProperty "Valid script context deserializes"           $ check1Valid   (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize"          $ check1Invalid (Proxy :: Proxy TokenName    ) True  False True
            , testProperty "Invalid redeemer does not deserialize"       $ check1Invalid (Proxy :: Proxy ()           ) False True  False
            , testProperty "Invalid script context does not deserialize" $ check1Invalid (Proxy :: Proxy ScriptContext) True  True  True
            ]
        , testGroup "Constraint 17. Payment authorized"
            [
              testProperty "Invalid authorization for withdrawal" $ checkWithdrawal True
            , testProperty "Missing authorized withdrawal"        $ checkWithdrawal False
            ]
        ]
    ]


-- | Check round-trip serialization of `Data`.
check1Valid :: (FromData a, ToData a, Eq a, Show a)
            => Gen a
            -> Property
check1Valid gen =
  property
    . forAll gen
    $ \x -> fromBuiltinData (toBuiltinData x) == Just x


-- | Check that invalid `BuiltinData` fails to deserialize to `Data`.
check1Invalid :: forall a
              . (FromData a, Eq a)
              => Proxy a   -- ^ The type for conversion to `BuiltinData`.
              -> Bool      -- ^ Whether to allow an empty list in the `Data`.
              -> Bool      -- ^ Whether to all byte strings in the `Data`.
              -> Bool      -- ^ Whether to allow `()` in the `Data`.
              -> Property  -- ^ The test property.
check1Invalid _ allowEmptyList allowByteString allowUnit =
  property
    $ let
        gen =
          let
            restrictEmptyList (BuiltinData (List [])) = allowEmptyList
            restrictEmptyList _                       = True
            restrictByteString (BuiltinData (B _)) = allowByteString
            restrictByteString _                   = True
            restrictUnit (BuiltinData (Constr 0 [])) = allowUnit
            restrictUnit _                           = True
          in
            -- FIXME: There is a very slight chance that a valid item might be generated at random.
            arbitrary `suchThat` (\x -> restrictEmptyList x && restrictByteString x && restrictUnit x)
      in
        forAll gen
          $ \x -> fromBuiltinData x == (Nothing :: Maybe a)


-- | Check that a semantics transaction succeeds.
checkSemanticsTransaction :: ArbitraryTransaction SemanticsTransaction ()      -- ^ Modifications to make before building the valid transaction.
                          -> ArbitraryTransaction SemanticsTransaction ()      -- ^ Modifications to make after building the valid transaction.
                          -> (PlutusTransaction SemanticsTransaction -> Bool)  -- ^ Whether to discard the transaction from the testing.
                          -> Bool                                              -- ^ Whether the transaction should test as valid.
                          -> Bool                                              -- ^ Whether to add noise to the script context.
                          -> Property                                          -- ^ The test property.
checkSemanticsTransaction modifyBefore modifyAfter condition valid noisy =
  property
    . forAll (arbitrarySemanticsTransaction modifyBefore modifyAfter noisy `suchThat` condition)
    $ \PlutusTransaction{..} ->
      case evaluateSemantics (_params _parameters) (toData _datum) (toData _redeemer) (toData _scriptContext) of
        This  e   -> not valid || (error $ show e)
        These e l -> not valid || (error $ show e <> ": " <> show l)
        That    _ -> valid


-- | Check that a payout transaction succeeds.
checkPayoutTransaction :: ArbitraryTransaction PayoutTransaction ()      -- ^ Modifications to make before building the valid transaction.
                       -> ArbitraryTransaction PayoutTransaction ()      -- ^ Modifications to make after building the valid transaction.
                       -> (PlutusTransaction PayoutTransaction -> Bool)  -- ^ Whether to discard the transaction from the testing.
                       -> Bool                                           -- ^ Whether the transaction should test as valid.
                       -> Bool                                           -- ^ Whether to add noise to the script context.
                       -> Property                                       -- ^ The test property.
checkPayoutTransaction modifyBefore modifyAfter condition valid noisy =
  property
    . forAll (arbitraryPayoutTransaction modifyBefore modifyAfter noisy `suchThat` condition)
    $ \PlutusTransaction{..} ->
      case evaluatePayout (_params' _parameters) (toData _datum) (toData _redeemer) (toData _scriptContext) of
        This  e   -> not valid || (error $ show e)
        These e l -> not valid || (error $ show e <> ": " <> show l)
        That    _ -> valid


-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: Property
checkDoubleInput =
  let
    modifyAfter =
      do
        ownAddress <- marloweParams `uses` semanticsAddress
        -- Create a random datum.
        inDatum <- lift arbitrary
        let inDatumHash = datumHash inDatum
        -- Create a random input to the script.
        inScript <-
          TxInInfo
            <$> lift arbitrary
            <*> (TxOut ownAddress <$> lift arbitrary <*> pure (Just inDatumHash))
        -- Add a second script input.
        infoInputs <>= [inScript]
        -- Add the new datum and its hash.
        infoData <>= [(inDatumHash, inDatum)]
        shuffle
  in
    checkSemanticsTransaction noModify modifyAfter noVeto False False


-- | Split a value in half.
splitValue :: Value -> [Value]
splitValue value =
  concat
    [
      [
        singleton c n half
      , singleton c n (i - half)
      ]
    |
      (c, n, i) <- flattenValue value
    , let half = i `div` 2
    ]


-- | Ensure that the contract closes the contract.
doesClose :: PlutusTransaction SemanticsTransaction -> Bool
doesClose = (== Close) . txOutContract . (^. output)


-- | Ensure that the transaction does not close the contract.
notCloses :: PlutusTransaction SemanticsTransaction -> Bool
notCloses = not . doesClose


-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: Property
checkMultipleOutput =
  let
    modifyAfter =
      do
        ownAddress <- marloweParams `uses` semanticsAddress
        let
          -- Split a script output into two equal ones.
          splitOwnOutput txOut@(TxOut address value datum')
            | address == ownAddress = flip (TxOut address) datum' <$> splitValue value
            | otherwise             = pure txOut
        -- Update the outputs with the split script output.
        infoOutputs %= concatMap splitOwnOutput
        shuffle
  in
    checkSemanticsTransaction noModify modifyAfter notCloses False False


-- | Check that validation fails if there is one Marlowe output upon close.
checkCloseOutput :: Property
checkCloseOutput =
  let
    modifyAfter =
      do
        ownAddress <- marloweParams `uses` semanticsAddress
        let
          -- Match the script input.
          matchOwnInput (TxInInfo _ (TxOut address _ _)) = address == ownAddress
        -- Find the script input.
        inScript <- infoInputs `uses` filter matchOwnInput
        -- Add a clone of the script input as output.
        infoOutputs <>= (txInInfoResolved <$> inScript)
        shuffle
  in
    checkSemanticsTransaction noModify modifyAfter doesClose
      True  -- FIXME: According to the specification, this test should fail.
      False


-- | Check that value input to a script matches its input state.
checkValueInput :: Property
checkValueInput =
  let
    modifyAfter =
      do
        ownAddress <- marloweParams `uses` semanticsAddress
        let
          -- Add one lovelace to the input to the script.
          incrementOwnInput txInInfo@(TxInInfo _ txOut@(TxOut address value _))
            | address == ownAddress = txInInfo {txInInfoResolved = txOut {txOutValue = value <> singleton adaSymbol adaToken 1}}
            | otherwise             = txInInfo
        -- Update the inputs with the incremented script input.
        infoInputs %= fmap incrementOwnInput
    in
      checkSemanticsTransaction noModify modifyAfter noVeto False False


-- | Check that value output to a script matches its output state.
checkValueOutput :: Property
checkValueOutput =
  let
    modifyAfter =
      do
        ownAddress <- marloweParams `uses` semanticsAddress
        let
          -- Add one lovelace to the output to the script.
          incrementOwnOutput txOut@(TxOut address value _)
            | address == ownAddress = txOut {txOutValue = value <> singleton adaSymbol adaToken 1}
            | otherwise             = txOut
        -- Update the outputs with the incremented script output.
        infoOutputs %= fmap incrementOwnOutput
  in
    checkSemanticsTransaction noModify modifyAfter notCloses False False


-- | Check that output datum to a script matches its semantic output.
checkDatumOutput :: (MarloweData -> Gen MarloweData) -> Property
checkDatumOutput perturb =
  let
    modifyAfter =
      do
        -- Find the existing Marlowe data output.
        marloweData <- MarloweData <$> output `uses` txOutState <*> output `uses` txOutContract
        -- Compute its hash.
        let outDatumHash = datumHash . Datum $ toBuiltinData marloweData
        -- Modify the original datum.
        outDatum' <- fmap (Datum . toBuiltinData) . lift $ perturb marloweData
        -- Let
        let
          -- Replace an output datum with the modification.
          perturbOwnOutputDatum pair@(h, _)
            | h == outDatumHash = (h, outDatum')
            | otherwise         = pair
        -- Update the data with the modification.
        infoData %= fmap perturbOwnOutputDatum
  in
    checkSemanticsTransaction noModify modifyAfter notCloses False False


-- | Check that state output to a script matches its output state.
checkStateOutput :: Property
checkStateOutput =
  checkDatumOutput
    $ \marloweData ->
      do
        -- Replace the output state with a random one.
        marloweState' <- arbitrary
        pure $ marloweData {marloweState = marloweState'}


-- | Check that state output to a script matches its output state.
checkContractOutput :: Property
checkContractOutput =
  checkDatumOutput
    $ \marloweData ->
      do
        -- Replace the output ccontact with a random one.
        marloweContract' <- arbitrary
        pure $ marloweData {marloweContract = marloweContract'}


-- | Check that non-positive accounts are rejected.
checkPositiveAccounts :: Property
checkPositiveAccounts =
  let
    modifyBefore =
      do
        -- Create a random non-positive entry for the accounts.
        account <- lift arbitrary
        token <- lift arbitrary
        amount' <- (1 -) <$> lift arbitraryPositiveInteger
        -- Add the non-positive entry to the accounts.
        inputState %= (\state -> state {accounts = AM.insert (account, token) amount' $ accounts state})
  in
    checkSemanticsTransaction modifyBefore noModify noVeto False False


-- | Compute the authorization for an input.
authorizer :: Input -> ([PubKeyHash], [TokenName])
authorizer (NormalInput     (IDeposit _ (PK   pkh  ) _ _        )    ) = (pure pkh, mempty    )
authorizer (NormalInput     (IDeposit _ (Role role') _ _        )    ) = (mempty  , pure role')
authorizer (NormalInput     (IChoice (ChoiceId _ (PK pkh    )) _)    ) = (pure pkh, mempty    )
authorizer (NormalInput     (IChoice (ChoiceId _ (Role role')) _)    ) = (mempty  , pure role')
authorizer (MerkleizedInput (IDeposit _ (PK   pkh  ) _ _        ) _ _) = (pure pkh, mempty    )
authorizer (MerkleizedInput (IDeposit _ (Role role') _ _        ) _ _) = (mempty  , pure role')
authorizer (MerkleizedInput (IChoice (ChoiceId _ (PK pkh    )) _) _ _) = (pure pkh, mempty    )
authorizer (MerkleizedInput (IChoice (ChoiceId _ (Role role')) _) _ _) = (mempty  , pure role')
authorizer _                                                           = (mempty  , mempty    )


-- | Determine whether there are any authorizations in the transaction.
hasAuthorizations :: PlutusTransaction SemanticsTransaction -> Bool
hasAuthorizations = (/= ([], [])) . bimap concat concat . unzip . fmap authorizer . txInputs . (^. input)


-- | Delete only the first matching value in a list.
deleteFirst :: (a -> Bool)  -- ^ The condition for deleting an element.
            -> [a]          -- ^ The list.
            -> [a]          -- ^ The list with the first matching element removed.
deleteFirst f z =
  case break f z of
    (x, []) -> x
    (x, y ) -> x <> tail y


-- | Check that a missing authorization causes failure.
checkAuthorization :: Property
checkAuthorization =
  let
    modifyAfter =
      do
        currency <- marloweParams `uses` rolesCurrency
        -- Determine the authorizations.
        (pkhs, roles) <- input `uses` (bimap concat concat . unzip . fmap authorizer . txInputs)
        let
          -- Determine whether a role token is present.
          matchRole TxInInfo{txInInfoResolved=TxOut{txOutValue}} = any (\role' -> valueOf txOutValue currency role' > 0) roles
          -- Determine whether a PKH authorization is present.
          matchPkh = (`elem` pkhs)
        -- Remove the first role token from the input.
        infoInputs %= deleteFirst matchRole
        -- Remove the first PKH signatory.
        infoSignatories %= deleteFirst matchPkh
  in
    checkSemanticsTransaction noModify modifyAfter hasAuthorizations False False


-- | Determine whether there are any external payments in a transaction.
hasExternalPayments :: PlutusTransaction SemanticsTransaction -> Bool
hasExternalPayments = any externalPayment . txOutPayments . ( ^. output)


-- | Determine whether a payment is external.
externalPayment :: Payment -> Bool
externalPayment (Payment _ (Party _) value) = value `gt` mempty
externalPayment _                           = False


-- | Decrement the value of each token by one.
decrementValue :: Value -> Value
decrementValue = foldMap (\(c, n, i) -> singleton c n (i - 1)) . flattenValue


-- | Check that an insufficient payment causes failure.
checkPayment :: Property
checkPayment =
  let
    modifyAfter =
      do
        -- Determine the payout address.
        payoutAddress' <- marloweParams `uses` payoutAddress
        let
          -- Decrement a payment by one unit.
          decrementPayment txOut@(TxOut address                          value (Just _))
            | address == payoutAddress'                                                  = txOut {txOutValue = decrementValue value}
            | otherwise                                                                  = txOut
          decrementPayment txOut@(TxOut (Address (PubKeyCredential _) _) value Nothing ) = txOut {txOutValue = decrementValue value}
          decrementPayment txOut                                                         = txOut
        -- Update the outputs.
        infoOutputs %= fmap decrementPayment
  in
    checkSemanticsTransaction noModify modifyAfter hasExternalPayments False False


-- | Remove a role input UTxOs from the transaction.
removeRoleIn :: ArbitraryTransaction PayoutTransaction ()
removeRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    let
      -- Determine if the input has the role token.
      notMatch (TxInInfo _ (TxOut _ value _)) = valueOf value currency name == 0
    -- Update the transaction inputs
    infoInputs %= filter notMatch


-- | Change the role name in an input UTxO from the transaction.
mutateRoleIn :: ArbitraryTransaction PayoutTransaction ()
mutateRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    -- Randomly choose a different name.
    name' <- lift $ arbitrary `suchThat` (/= name)
    let
      -- Mutate the roles currency.
      mutate txIn@(TxInInfo _ (TxOut _ value _)) =
        if valueOf value currency name /= 0
          then txIn {txInInfoResolved = (txInInfoResolved txIn) {txOutValue = singleton currency name' 1}}
          else txIn
    -- Update the transaction inputs
    infoInputs %= fmap mutate


-- | Check that an invalid withdrawal transaction fails.
checkWithdrawal :: Bool
                -> Property
checkWithdrawal mutate =
  checkPayoutTransaction noModify
    (if mutate then mutateRoleIn else removeRoleIn)
    noVeto False False
