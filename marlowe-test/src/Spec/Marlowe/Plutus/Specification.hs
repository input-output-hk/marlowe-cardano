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


{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Spec.Marlowe.Plutus.Specification
  ( -- * Testing
    tests
  ) where


import Control.Lens (use, uses, (%=), (<>=), (<~), (^.))
import Control.Monad.State (lift)
import Data.Bifunctor (bimap)
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(..))
import Data.These (These(That, These, This))
import Language.Marlowe.Core.V1.Semantics
  ( MarloweData(MarloweData, marloweContract, marloweState)
  , MarloweParams(rolesCurrency)
  , Payment(Payment)
  , TransactionInput(..)
  , TransactionOutput(txOutContract, txOutPayments, txOutState)
  , totalBalance
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( ChoiceId(ChoiceId)
  , Contract(..)
  , Input(..)
  , InputContent(IChoice, IDeposit)
  , Party(Role)
  , Payee(Party)
  , State(accounts)
  )
import Language.Marlowe.Scripts (MarloweInput)
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Address (toPubKeyHash)
import Plutus.V1.Ledger.Value (flattenValue, valueOf)
import Plutus.V2.Ledger.Api
  ( Address(Address)
  , BuiltinData(BuiltinData)
  , Credential(PubKeyCredential)
  , Data(B, Constr, List)
  , Datum(..)
  , DatumHash(DatumHash)
  , FromData(..)
  , LogOutput
  , OutputDatum(..)
  , PubKeyHash
  , ScriptContext
  , ToData(..)
  , TokenName
  , TxInInfo(TxInInfo, txInInfoResolved)
  , TxOut(TxOut, txOutValue)
  , ValidatorHash
  , Value
  , adaSymbol
  , adaToken
  , singleton
  , toData
  )
import Spec.Marlowe.Plutus.Lens ((<><~))
import Spec.Marlowe.Plutus.Script
  (evaluatePayout, evaluateSemantics, payoutAddress, payoutScriptHash, semanticsAddress, semanticsScriptHash)
import Spec.Marlowe.Plutus.Transaction
  ( ArbitraryTransaction
  , arbitraryPayoutTransaction
  , arbitrarySemanticsTransaction
  , isScriptTxIn
  , merkleize
  , noModify
  , noVeto
  , shuffle
  )
import Spec.Marlowe.Plutus.Types
  ( PayoutTransaction
  , PlutusTransaction(..)
  , SemanticsTransaction
  , infoData
  , infoInputs
  , infoOutputs
  , infoSignatories
  , input
  , inputState
  , marloweParams
  , marloweParamsPayout
  , output
  , role
  )
import Spec.Marlowe.Reference (ReferencePath)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary(..)
  , Gen
  , Property
  , chooseInteger
  , elements
  , forAll
  , listOf1
  , oneof
  , property
  , suchThat
  , testProperty
  , (===)
  )

import qualified Language.Marlowe.Core.V1.Semantics as M (MarloweData(marloweParams))
import qualified Language.Marlowe.Core.V1.Semantics.Types as M (Party(Address), State(..))
import qualified PlutusTx.AssocMap as AM (Map, fromList, insert, keys, null, toList)
import qualified Test.Tasty.QuickCheck as Q (shuffle)


-- | Conditionally check Plutus trace log messages.

checkPlutusLog :: Bool
#ifdef TRACE_PLUTUS
checkPlutusLog = True
#else
checkPlutusLog = False
#endif


-- | Run tests.
tests :: [ReferencePath] -> TestTree
tests referencePaths =
  testGroup "Marlowe On-Chain Specification"
    [
      testGroup "Semantics Validator"
        [
          testGroup "Valid transaction succeeds"
            [
              testProperty "Noiseless" $ checkSemanticsTransaction mempty referencePaths noModify noModify noVeto True False True
            , testProperty "Noisy"     $ checkSemanticsTransaction mempty referencePaths noModify noModify noVeto True True  True
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
              testProperty "Invalid attempt to run two Marlowe scripts" $ checkDoubleInput referencePaths
            ]
        , testGroup "Constraint 3. Single Marlowe output"
            [
              testProperty "Invalid attempt to split Marlowe output" $ checkMultipleOutput referencePaths
            ]
        , testGroup "Constraint 4. No output to script on close"
            [
              testProperty "Invalid attempt to output to Marlowe on close" $ checkCloseOutput referencePaths
            ]
        , testGroup "Constraint 5. Input value from script"
            [
              testProperty "Invalid mismatch between state and script input" $ checkValueInput referencePaths
            ]
        , testGroup "Constraint 6. Output value to script"
            [
              testProperty "Invalid mismatch between expected and actual output to script" $ checkValueOutput referencePaths
            ]
        , testGroup "Constraint 7. Input state"
            [
              -- TODO: This test requires instrumenting the Plutus script. For now, this constraint is enforced manually by code inspection.
            ]
        , testGroup "Constraint 8. Input contract"
            [
              -- TODO: This test requires instrumenting the Plutus script. For now, this constraint is enforced manually by code inspection.
            ]
        , testGroup "Constraint 9. Marlowe parameters"
            [
              testProperty "Invalid alteration of parameters" $ checkParamsOutput referencePaths
            ]
        , testGroup "Constraint 10. Output state"
            [
              testProperty "Invalid mismatch between state and script output's state" $ checkStateOutput referencePaths
            ]
        , testGroup "Constraint 11. Output contract"
            [
              testProperty "Invalid mismatch between contract and script output" $ checkContractOutput referencePaths
            ]
        , testGroup "Constraint 12. Merkleized continuations"
            [
              testProperty "Valid merkleization"   $ checkMerkleization referencePaths True
            , testProperty "Invalid merkleization" $ checkMerkleization referencePaths False
            ]
        , testGroup "Constraint 13. Positive balances"
            [
              testProperty "Invalid non-positive balance" $ checkPositiveAccounts referencePaths
              -- TODO: This test on the output state requires instrumenting the Plutus script. For now, this constraint is enforced manually by code inspection.
            ]
        , testGroup "Constraint 14. Inputs authorized"
            [
              testProperty "Invalid missing authorization" $ checkAuthorization referencePaths
            ]
        , testGroup "Constraint 15. Sufficient payment"
            [
              testProperty "Invalid insufficient payment" $ checkPayment referencePaths
            ]
        , testGroup "Constraint 18. Final balance"
            [
              testProperty "Invalid mismatch between output value and state" $ checkOutputConsistency referencePaths
            ]
        , testGroup "Constraint 19. No duplicates"
            [
              testProperty "Invalid duplicate accounts in input state" $ checkInputDuplicates referencePaths
              -- TODO: This test on the output state requires instrumenting the Plutus script. For now, this constraint is enforced manually by code inspection.
            ]
        , testGroup "Constraint 20. Single satisfaction"
            [
              testProperty "Invalid other validators during payment" $ checkOtherValidators referencePaths
            ]
        , testProperty "Script hash matches reference hash"
            $ checkValidatorHash semanticsScriptHash
              -- DO NOT ALTER THE FOLLOWING VALUE UNLESS YOU ARE COMMITTING
              -- APPROVED CHANGES TO MARLOWE'S SEMANTICS VALIDATOR. THIS HASH
              -- HAS IMPLICATIONS FOR VERSIONING, AUDIT, AND CONTRACT DISCOVERY.
              (
                if checkPlutusLog
                  then "c6db3a4f2e08ce10cd34aa1cd06f97730f5d55e0fcc20e3cb5149ea4"
                  else "2ed2631dbb277c84334453c5c437b86325d371f0835a28b910a91a6e"
              )
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
        , testProperty "Script hash matches reference hash"
            $ checkValidatorHash payoutScriptHash
              -- DO NOT ALTER THE FOLLOWING VALUE UNLESS YOU ARE COMMITTING
              -- APPROVED CHANGES TO MARLOWE'S ROLE VALIDATOR. THIS HASH HAS
              -- IMPLICATIONS FOR VERSIONING, AUDIT, AND CONTRACT DISCOVERY.
              "e165610232235bbbbeff5b998b233daae42979dec92a6722d9cda989"
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
            -- TODO: There is a very slight chance that a valid item might be generated at random.
            arbitrary `suchThat` (\x -> restrictEmptyList x && restrictByteString x && restrictUnit x)
      in
        forAll gen
          $ \x -> fromBuiltinData x == (Nothing :: Maybe a)


-- | Check that a semantics transaction succeeds.
checkSemanticsTransaction :: LogOutput                                         -- ^ At least one of these required log messages must be reported if the validation fails.
                          -> [ReferencePath]                                   -- ^ The reference execution paths from which to choose.
                          -> ArbitraryTransaction SemanticsTransaction ()      -- ^ Modifications to make before building the valid transaction.
                          -> ArbitraryTransaction SemanticsTransaction ()      -- ^ Modifications to make after building the valid transaction.
                          -> (PlutusTransaction SemanticsTransaction -> Bool)  -- ^ Whether to discard the transaction from the testing.
                          -> Bool                                              -- ^ Whether the transaction should test as valid.
                          -> Bool                                              -- ^ Whether to add noise to the script context.
                          -> Bool                                              -- ^ Whether to allow merkleization.
                          -> Property                                          -- ^ The test property.
checkSemanticsTransaction requiredLog referencePaths modifyBefore modifyAfter condition valid noisy allowMerkleization =
  property
    . forAll (arbitrarySemanticsTransaction referencePaths modifyBefore modifyAfter noisy allowMerkleization `suchThat` condition)
    $ \PlutusTransaction{..} ->
      case evaluateSemantics (toData _datum) (toData _redeemer) (toData _scriptContext) of
        This  e   -> not valid || error (show e)
        These e l -> not valid && matchesPlutusLog l || error (show e <> ": " <> show l)
        That    _ -> valid
    where
      matchesPlutusLog l = not checkPlutusLog || any (`elem` l) requiredLog


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
      case evaluatePayout (toData _datum) (toData _redeemer) (toData _scriptContext) of
        This  e   -> not valid || error (show e)
        These e l -> not valid || error (show e <> ": " <> show l)
        That    _ -> valid


-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: [ReferencePath] -> Property
checkDoubleInput referencePaths =
  let
    modifyAfter =
      do
        -- Create a random datum.
        inDatum <- lift arbitrary
        let inDatumHash = datumHash inDatum
        -- Create a random input to the script.
        inScript <-
          TxInInfo
            <$> lift arbitrary
            <*> (TxOut semanticsAddress <$> lift arbitrary <*> pure (OutputDatumHash inDatumHash) <*> pure Nothing)
        -- Add a second script input.
        infoInputs <>= [inScript]
        -- Add the new datum and its hash.
        infoData <>= AM.fromList [(inDatumHash, inDatum)]
        shuffle
  in
    checkSemanticsTransaction ["w"] referencePaths noModify modifyAfter noVeto False False False


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


-- | Ensure that the contract makes payments.
hasPayouts :: PlutusTransaction SemanticsTransaction -> Bool
hasPayouts =
  let
    isPayout (Payment _ (Party _) _ i) = i > 0
    isPayout _ = False
  in
    any isPayout . txOutPayments . (^. output)


-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: [ReferencePath] -> Property
checkMultipleOutput referencePaths =
  let
    modifyAfter =
      do
        let
          -- Split a script output into two equal ones.
          splitOwnOutput txOut@(TxOut address value datum' _)
            | address == semanticsAddress = flip (TxOut address) datum' <$> splitValue value <*> pure Nothing
            | otherwise                   = pure txOut
        -- Update the outputs with the split script output.
        infoOutputs %= concatMap splitOwnOutput
        shuffle
  in
    checkSemanticsTransaction ["o"] referencePaths noModify modifyAfter notCloses False False False


-- | Check that validation fails if there is one Marlowe output upon close.
checkCloseOutput :: [ReferencePath] -> Property
checkCloseOutput referencePaths =
  let
    modifyAfter =
      do
        let
          -- Match the script input.
          matchOwnInput (TxInInfo _ (TxOut address _ _ _)) = address == semanticsAddress
        -- Find the script input.
        inScript <- infoInputs `uses` filter matchOwnInput
        -- Add a clone of the script input as output.
        infoOutputs <>= (txInInfoResolved <$> inScript)
        shuffle
  in
    checkSemanticsTransaction ["c"] referencePaths noModify modifyAfter doesClose False False False


-- | Check that value input to a script matches its input state.
checkValueInput :: [ReferencePath] -> Property
checkValueInput referencePaths =
  let
    modifyAfter =
      do
        let
          -- Add one lovelace to the input to the script.
          incrementOwnInput txInInfo@(TxInInfo _ txOut@(TxOut address value _ _))
            | address == semanticsAddress = txInInfo {txInInfoResolved = txOut {txOutValue = value <> singleton adaSymbol adaToken 1}}
            | otherwise                   = txInInfo
        -- Update the inputs with the incremented script input.
        infoInputs %= fmap incrementOwnInput
    in
      checkSemanticsTransaction ["vi"] referencePaths noModify modifyAfter noVeto False False False


-- | Check that value output to a script matches its expectation.
checkValueOutput :: [ReferencePath] -> Property
checkValueOutput referencePaths =
  let
    modifyAfter =
      do
        delta <- lift $ oneof [chooseInteger (-5, -1), chooseInteger (1, 5), arbitrary `suchThat` (/= 0)]  -- Ensure small non-zero integers.
        let
          -- Add or subtract some lovelace to the output to the script.
          incrementOwnOutput txOut@(TxOut address value _ _)
            | address == semanticsAddress = txOut {txOutValue = value <> singleton adaSymbol adaToken delta}
            | otherwise                   = txOut
        -- Update the outputs with the incremented script output.
        infoOutputs %= fmap incrementOwnOutput
  in
    checkSemanticsTransaction ["d"] referencePaths noModify modifyAfter notCloses False False False


-- | Check the consistency of the output value with the output state.
checkOutputConsistency :: [ReferencePath] -> Property
checkOutputConsistency referencePaths =
  property
    . forAll (arbitrarySemanticsTransaction referencePaths noModify noModify False True)
    $ \tx ->
      let
        findOwnOutput (TxOut address value _ _)
          | address == semanticsAddress = value
          | otherwise                   = mempty
        outValue = foldMap findOwnOutput $ tx ^. infoOutputs
        finalBalance = totalBalance . accounts . txOutState $ tx ^. output
        -- There is really no way to provoke this invalidity in a manner that isn't covered by other tests.
        valid = outValue == finalBalance
      in
        checkSemanticsTransaction [] referencePaths noModify noModify notCloses valid False False


-- | Add a duplicate entry to an assocation list.
addDuplicate :: Arbitrary v => AM.Map k v -> Gen (AM.Map k v)
addDuplicate am =
  do
    let
      am' = AM.toList am
    key <- elements $ fst <$> am'
    value <- arbitrary
    AM.fromList <$> Q.shuffle ((key, value) : am')


-- | Check for the detection of duplicates in input state
checkInputDuplicates :: [ReferencePath] -> Property
checkInputDuplicates referencePaths =
  let
    hasDuplicates tx =
      let
        hasDuplicate am = length (AM.keys am) /= length (nub $ AM.keys am)
        M.State{..} = tx ^. inputState
      in
           hasDuplicate accounts
        || hasDuplicate choices
        || hasDuplicate boundValues
    makeDuplicates am =
      if AM.null am
        then pure am
        else oneof [pure am, addDuplicate am]
    modifyBefore =
      do
        M.State{..} <- use inputState
        inputState <~
          lift
            (
              M.State
                <$> makeDuplicates accounts
                <*> makeDuplicates choices
                <*> makeDuplicates boundValues
                <*> pure minTime
            )
  in
    checkSemanticsTransaction ["bi", "eai", "ebi", "eci", "n"] referencePaths modifyBefore noModify hasDuplicates False False False


-- | Check that output datum to a script matches its semantic output.
checkDatumOutput :: [ReferencePath] -> (MarloweData -> Gen MarloweData) -> Property
checkDatumOutput referencePaths perturb =
  let
    modifyAfter =
      do
        -- Find the existing Marlowe data output.
        marloweData <- MarloweData <$> use marloweParams <*> output `uses` txOutState <*> output `uses` txOutContract
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
        infoData %= AM.fromList . fmap perturbOwnOutputDatum . AM.toList
  in
    checkSemanticsTransaction ["d"] referencePaths noModify modifyAfter notCloses False False False


-- | Check that other validators are forbidden during payments.
checkOtherValidators :: [ReferencePath] -> Property
checkOtherValidators referencePaths =
  let
    modifyAfter =
      -- Add an extra script input.
      infoInputs <><~ lift (listOf1 $ arbitrary `suchThat` isScriptTxIn)
  in
    checkSemanticsTransaction ["z"] referencePaths noModify modifyAfter hasPayouts False False False


-- | Check that parameters in the datum are not changed by the transaction.
checkParamsOutput :: [ReferencePath] -> Property
checkParamsOutput referencePaths =
  checkDatumOutput referencePaths
    $ \marloweData ->
      do
        -- Replace the output parameters with a random one.
        let old = M.marloweParams marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData {M.marloweParams = new}


-- | Check that state output to a script matches its semantic output.
checkStateOutput :: [ReferencePath] -> Property
checkStateOutput referencePaths =
  checkDatumOutput referencePaths
    $ \marloweData ->
      do
        -- Replace the output state with a random one.
        let old = marloweState marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData {marloweState = new}


-- | Check that contract output to a script matches its semantic output.
checkContractOutput :: [ReferencePath] -> Property
checkContractOutput referencePaths =
  checkDatumOutput referencePaths
    $ \marloweData ->
      do
        -- Replace the output ccontact with a random one.
        let old = marloweContract marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData {marloweContract = new}


-- | Check that the input contract is merkleized.
hasMerkleizedInput :: PlutusTransaction SemanticsTransaction -> Bool
hasMerkleizedInput =
  let
    isMerkleized NormalInput{}     = False
    isMerkleized MerkleizedInput{} = True
  in
    any isMerkleized . txInputs . (^. input)


-- | Check than an invalid merkleization is rejected.
checkMerkleization :: [ReferencePath] -> Bool -> Property
checkMerkleization referencePaths valid =
  let
    -- Merkleizedd the contract and its input.
    modifyBefore = merkleize
    -- Extract the merkle hash, if any.
    merkleHash (NormalInput _)            = mempty
    merkleHash (MerkleizedInput _ hash _) = pure $ DatumHash hash
    -- Modify the contract if requested.
    modifyAfter =
      if valid
        then pure ()
        else do
               -- Remove the merkleized continuation datums for the input.
               hashes <- input `uses` (concatMap merkleHash . txInputs)
               infoData %= (AM.fromList . filter ((`notElem` hashes) . fst) . AM.toList)
  in
    checkSemanticsTransaction ["h"] referencePaths modifyBefore modifyAfter hasMerkleizedInput valid False False


-- | Check that non-positive accounts are rejected.
checkPositiveAccounts :: [ReferencePath] -> Property
checkPositiveAccounts referencePaths =
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
    checkSemanticsTransaction ["bi"] referencePaths modifyBefore noModify noVeto False False False


-- | Compute the authorization for an input.
authorizer :: Input -> ([PubKeyHash], [TokenName])
authorizer (NormalInput     (IDeposit _ (M.Address _ address) _ _        )    ) = (maybeToList $ toPubKeyHash address, mempty    )
authorizer (NormalInput     (IDeposit _ (Role role'         ) _ _        )    ) = (mempty                            , pure role')
authorizer (NormalInput     (IChoice (ChoiceId _ (M.Address _ address)) _)    ) = (maybeToList $ toPubKeyHash address, mempty    )
authorizer (NormalInput     (IChoice (ChoiceId _ (Role role'         )) _)    ) = (mempty                            , pure role')
authorizer (MerkleizedInput (IDeposit _ (M.Address _ address) _ _        ) _ _) = (maybeToList $ toPubKeyHash address, mempty    )
authorizer (MerkleizedInput (IDeposit _ (Role role'       ) _ _          ) _ _) = (mempty                            , pure role')
authorizer (MerkleizedInput (IChoice (ChoiceId _ (M.Address _ address)) _) _ _) = (maybeToList $ toPubKeyHash address, mempty    )
authorizer (MerkleizedInput (IChoice (ChoiceId _ (Role role'         )) _) _ _) = (mempty                            , pure role')
authorizer _                                                                    = (mempty                            , mempty    )


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
checkAuthorization :: [ReferencePath] -> Property
checkAuthorization referencePaths =
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
    checkSemanticsTransaction ["s", "t"] referencePaths noModify modifyAfter hasAuthorizations False False False


-- | Determine whether there are any external payments in a transaction.
hasExternalPayments :: PlutusTransaction SemanticsTransaction -> Bool
hasExternalPayments = any externalPayment . txOutPayments . ( ^. output)


-- | Determine whether a payment is external.
externalPayment :: Payment -> Bool
externalPayment (Payment _ (Party _) _ amount) = amount > 0
externalPayment _                              = False


-- | Decrement the value of each token by one.
decrementValue :: Value -> Value
decrementValue = foldMap (\(c, n, i) -> singleton c n (i - 1)) . flattenValue


-- | Check that an insufficient payment causes failure.
checkPayment :: [ReferencePath] -> Property
checkPayment referencePaths =
  let
    modifyAfter =
      do
        let
          -- Decrement a payment by one unit.
          decrementPayment txOut@(TxOut address                          value (OutputDatumHash _) _)
            | address == payoutAddress                                                                = txOut {txOutValue = decrementValue value}
            | otherwise                                                                               = txOut
          decrementPayment txOut@(TxOut (Address (PubKeyCredential _) _) value NoOutputDatum       _) = txOut {txOutValue = decrementValue value}
          decrementPayment txOut                                                                      = txOut
        -- Update the outputs.
        infoOutputs %= fmap decrementPayment
  in
    checkSemanticsTransaction ["p", "r"] referencePaths noModify modifyAfter hasExternalPayments False False False


-- | Remove a role input UTxOs from the transaction.
removeRoleIn :: ArbitraryTransaction PayoutTransaction ()
removeRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    let
      -- Determine if the input has the role token.
      notMatch (TxInInfo _ (TxOut _ value _ _ )) = valueOf value currency name == 0
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
      mutate txIn@(TxInInfo _ (TxOut _ value _ _ )) =
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


-- | Check that a validator hash is correct.
checkValidatorHash :: ValidatorHash
                   -> ValidatorHash
                   -> Property
checkValidatorHash actual reference =
  property $ actual === reference
