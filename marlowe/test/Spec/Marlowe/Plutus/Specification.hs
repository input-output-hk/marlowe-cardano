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


import Control.Lens ((^.))
import Control.Lens.Tuple (_3, _5)
import Data.Bifunctor (bimap)
import Data.List (permutations)
import Data.Proxy (Proxy (..))
import Data.These (These (That, These, This))
import Language.Marlowe.Core.V1.Semantics (MarloweData (marloweContract, marloweState),
                                           MarloweParams (MarloweParams, rolePayoutValidatorHash, rolesCurrency),
                                           Payment (Payment), TransactionOutput (txOutContract, txOutPayments))
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId (ChoiceId), Contract (Close),
                                                 InputContent (IChoice, IDeposit), Party (PK, Role), Payee (Party),
                                                 State (accounts))
import Language.Marlowe.Scripts (MarloweInput, MarloweTxInput (Input, MerkleizedTxInput))
import Plutus.V1.Ledger.Api (Address (Address), BuiltinData (BuiltinData), Credential (PubKeyCredential),
                             Data (B, Constr, List), Datum (Datum), FromData (..), ScriptContext (scriptContextTxInfo),
                             ToData (..), TokenName, TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                             TxInfo (txInfoData, txInfoInputs, txInfoOutputs, txInfoSignatories),
                             TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue), adaSymbol, adaToken, singleton,
                             toData)
import Plutus.V1.Ledger.Value (flattenValue, gt, valueOf)
import Spec.Marlowe.Plutus.Arbitrary (SemanticsTest, SemanticsTest', arbitraryPayoutTransaction,
                                      arbitrarySemanticsTransaction, arbitrarySemanticsTransactionModifyState)
import Spec.Marlowe.Plutus.Script (evaluatePayout, evaluateSemantics, payoutAddress, semanticsAddress)
import Spec.Marlowe.Plutus.Types ()
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, elements, forAll, property, suchThat, testProperty)

import qualified PlutusTx.AssocMap as AM


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Marlowe On-Chain Specification"
    [
      testGroup "Semantics Validator"
        [
          testGroup "Valid transaction succeeds"
            [
              testProperty "Noiseless" $ checkSemanticsTransaction False
            , testProperty "Noisy"     $ checkSemanticsTransaction True
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
        , testGroup "Constraint 4. No output to script on close"
            [
              testProperty "Invalid attempt to output to Marlowe on close" checkCloseOutput
            |
              False  -- FIXME: Settle whether to include this failing test in the specification.
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
              testProperty "Noiseless" $ checkPayoutTransaction False
            , testProperty "Noisy"     $ checkPayoutTransaction True
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
check1Valid :: (FromData a, ToData a, Eq a, Show a) => Gen a -> Property
check1Valid gen =
  property
    . forAll gen
    $ \x -> fromBuiltinData (toBuiltinData x) == Just x


-- | Check that invalid `BuiltinData` fails to deserialize to `Data`.
check1Invalid :: forall a . (FromData a, Eq a) => Proxy a -> Bool -> Bool -> Bool -> Property
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


-- | Check that a semantically valid transaction succeeds.
checkSemanticsTransaction :: Bool -> Property
checkSemanticsTransaction noisy =
  property
    . forAll (arbitrarySemanticsTransaction noisy)
    $ \(marloweParams, marloweData, marloweInput, scriptContext, _) ->
      case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
        This  e   -> error $ show e
        These e l -> error $ show (e, l)
        That    _ -> True


-- | Check that a valid payout transaction succeeds.
checkPayoutTransaction :: Bool -> Property
checkPayoutTransaction noisy =
  property
    . forAll (arbitraryPayoutTransaction noisy)
    $ \(marloweParams, role, scriptContext) ->
      case evaluatePayout marloweParams (toData role) (toData ()) (toData scriptContext) of
        This  e   -> error $ show e
        These e l -> error $ show e <> ": " <> show l
        That    _ -> True


-- | Check that an invalid semantics transaction fails.
checkInvalidSemantics :: (SemanticsTest -> Bool)
                      -> (SemanticsTest' -> Gen TxInfo)
                      -> Property
checkInvalidSemantics condition modify =
  property
    $ let
        gen =
          do
            (marloweParams, marloweData, marloweInput, scriptContext, output)
              <- arbitrarySemanticsTransaction False `suchThat` condition
            txInfo' <- modify (marloweParams, marloweData, marloweInput, scriptContextTxInfo scriptContext, output)
            pure (marloweParams, marloweData, marloweInput, scriptContext {scriptContextTxInfo = txInfo'})
      in
        forAll gen
          $ \(marloweParams, marloweData, marloweInput, scriptContext) ->
            case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
              That{} -> False
              _      -> True


-- | Check that an invalid semantics transaction fails.
checkInvalidSemanticsModifyState :: (SemanticsTest -> Bool)
                                 -> (State -> Gen State)
                                 -> Property
checkInvalidSemanticsModifyState condition modify =
  property
    . forAll (arbitrarySemanticsTransactionModifyState modify False `suchThat` condition)
      $ \(marloweParams, marloweData, marloweInput, scriptContext, _) ->
        case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
          That{} -> False
          _      -> True


-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: Property
checkDoubleInput =
  checkInvalidSemantics (const True)
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        txInInfoOutRef <- arbitrary
        txOutValue <- arbitrary
        txOutDatumHash <- Just <$> arbitrary
        let
          txOutAddress = semanticsAddress marloweParams
          txInInfoResolved = TxOut{..}
        txInfoInputs' <- elements . permutations $ TxInInfo{..} : txInfoInputs txInfo
        pure $ txInfo {txInfoInputs = txInfoInputs'}


-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: Property
checkMultipleOutput =
  checkInvalidSemantics ((/= Close) . txOutContract . (^. _5))
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          outputs = txInfoOutputs txInfo
          matchOwnOutput (TxOut address _ _) = address == ownAddress
          ownOutputs' =
            concat
              [
                let
                  (half, half') =
                    bimap mconcat mconcat
                      $ unzip
                      [(singleton c n (i `div` 2), singleton c n (i - (i `div` 2))) | (c, n, i) <- flattenValue value]
                in
                  [TxOut address half datum, TxOut address half' datum]
              |
                TxOut address value datum <- filter matchOwnOutput outputs
              ]
        txInfoOutputs' <- elements . permutations $ filter (not . matchOwnOutput) outputs <> ownOutputs'
        pure $ txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that validation fails if there is more than one Marlowe output.
checkCloseOutput :: Property
checkCloseOutput =
  checkInvalidSemantics ((== Close) . txOutContract . (^. _5))
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          matchOwnInput (TxInInfo _ (TxOut address _ _)) = address == ownAddress
          inputs = txInfoInputs txInfo
          outputs = txInfoOutputs txInfo
          ownOutputs =
            [
              txOut
            |
              TxInInfo _ txOut <- filter matchOwnInput inputs
            ]
        txInfoOutputs' <- elements . permutations $ outputs <> ownOutputs
        pure $ txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that value input to a script matches its input state.
checkValueInput :: Property
checkValueInput =
  checkInvalidSemantics (const True)
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          txInfoInputs' =
            [
              if address == ownAddress
                then txInInfo {txInInfoResolved = txOut {txOutValue = value <> singleton adaSymbol adaToken 1}}
                else txInInfo
            |
              txInInfo@(TxInInfo _ txOut@(TxOut address value _)) <- txInfoInputs txInfo
            ]
        pure $ txInfo {txInfoInputs = txInfoInputs'}


-- | Check that value output to a script matches its output state.
checkValueOutput :: Property
checkValueOutput =
  checkInvalidSemantics ((/= Close) . txOutContract . (^. _5))
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          txInfoOutputs' =
            [
              if address == ownAddress
                then txOut {txOutValue = value <> singleton adaSymbol adaToken 1}
                else txOut
            |
              txOut@(TxOut address value _) <- txInfoOutputs txInfo
            ]
        pure $ txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that output datum to a script matches its semantic output.
checkDatumOutput :: (MarloweData -> Gen MarloweData) -> Property
checkDatumOutput modify =
  checkInvalidSemantics ((/= Close) . txOutContract . (^. _5))
    $ \(marloweParams, marloweData, _, txInfo, _) ->
      do
        marloweData' <- modify marloweData
        let
          ownAddress = semanticsAddress marloweParams
          outputHash =
            [
              h
            |
              TxOut address _ h <- txInfoOutputs txInfo
            , address == ownAddress
            ]
          txInfoData' =
            [
              if Just h `elem` outputHash
                then (h, Datum $ toBuiltinData marloweData')
                else pair
            |
              pair@(h, _) <- txInfoData txInfo
            ]
        pure $ txInfo {txInfoData = txInfoData'}


-- | Check that state output to a script matches its output state.
checkStateOutput :: Property
checkStateOutput =
  checkDatumOutput
    $ \marloweData ->
      do
        marloweState' <- arbitrary
        pure $ marloweData {marloweState = marloweState'}


-- | Check that state output to a script matches its output state.
checkContractOutput :: Property
checkContractOutput =
  checkDatumOutput
    $ \marloweData ->
      do
        marloweContract' <- arbitrary
        pure $ marloweData {marloweContract = marloweContract'}


-- | Check that non-positive accounts are rejected.
checkPositiveAccounts :: Property
checkPositiveAccounts =
  checkInvalidSemanticsModifyState (const True)
    $ \state ->
      do
        account <- arbitrary
        token <- arbitrary
        amount <- (1 -) <$> arbitraryPositiveInteger
        pure $ state {accounts = AM.insert (account, token) amount $ accounts state}


-- | Check that a missing authorization causes failure.
checkAuthorization :: Property
checkAuthorization =
  let
    authorizations inputs =
      bimap concat concat
        $ unzip
        [
          case input of
            Input             (IDeposit _ (PK   pkh ) _ _        )   -> ([pkh], [    ])
            Input             (IDeposit _ (Role role) _ _        )   -> ([   ], [role])
            Input             (IChoice (ChoiceId _ (PK pkh   )) _)   -> ([pkh], [    ])
            Input             (IChoice (ChoiceId _ (Role role)) _)   -> ([   ], [role])
            MerkleizedTxInput (IDeposit _ (PK   pkh ) _ _        ) _ -> ([pkh], [    ])
            MerkleizedTxInput (IDeposit _ (Role role) _ _        ) _ -> ([   ], [role])
            MerkleizedTxInput (IChoice (ChoiceId _ (PK pkh   )) _) _ -> ([pkh], [    ])
            MerkleizedTxInput (IChoice (ChoiceId _ (Role role)) _) _ -> ([   ], [role])
            _                                                        -> ([   ], [    ])
        |
          input <- inputs
        ]
  in
    checkInvalidSemantics ((/= ([], [])) . authorizations . (^. _3))
      $ \(MarloweParams{..}, _, input, txInfo, _) ->
        pure
          $ let
              (pkhs, roles) = authorizations input
              filterOne f z =
                case break f z of
                  (x, []) -> x
                  (x, y ) -> x <> tail y
              txInfoInputs' =
                filterOne (\TxInInfo{txInInfoResolved=TxOut{txOutValue}} -> any (\role -> valueOf txOutValue rolesCurrency role > 0) roles)
                  $ txInfoInputs txInfo
              txInfoSignatories' = filterOne (`elem` pkhs) $ txInfoSignatories txInfo
            in
              txInfo {txInfoInputs = txInfoInputs', txInfoSignatories = txInfoSignatories'}


-- | Check that an insufficient payment causes failure.
checkPayment :: Property
checkPayment =
  let
    externalPayment (Payment _ (Party _) value) = value `gt` mempty
    externalPayment _                           = False
  in
    checkInvalidSemantics (any externalPayment . txOutPayments . (^. _5))
      $ \(marloweParams, _, _, txInfo, _) ->
        pure
          $ let
              reduce value = foldMap (\(c, n, i) -> singleton c n (i - 1)) $ flattenValue value
              txInfoOutputs' =
                [
                  case (txOutAddress txOut == payoutAddress marloweParams, txOut) of
                    (False, TxOut (Address (PubKeyCredential _) _) value Nothing ) -> txOut {txOutValue = reduce value}
                    (True , TxOut _                                value (Just _)) -> txOut {txOutValue = reduce value}
                    _                                                              -> txOut
                |
                  txOut <- txInfoOutputs txInfo
                ]
            in
              txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that an invalid semantics transaction fails.
checkWithdrawal :: Bool -> Property
checkWithdrawal mutate =
  property
    $ let
        gen =
          do
            (marloweParams, role, scriptContext) <- arbitraryPayoutTransaction False
            role' <- arbitrary
            let
              txInfoInputs' =
                [
                  if mutate && isRole
                    then txInInfo {txInInfoResolved = (txInInfoResolved txInInfo) {txOutValue = singleton (rolesCurrency marloweParams) role' 1}}
                    else txInInfo
                |
                  txInInfo <- txInfoInputs $ scriptContextTxInfo scriptContext
                , let isRole = valueOf (txOutValue (txInInfoResolved txInInfo)) (rolesCurrency marloweParams) role == 0
                , not mutate && isRole
                ]
              scriptContext' = scriptContext {scriptContextTxInfo = (scriptContextTxInfo scriptContext) {txInfoInputs = txInfoInputs'}}
            pure (marloweParams, role, scriptContext')
      in
        forAll gen
          $ \(marloweParams, role, scriptContext) ->
            case evaluatePayout marloweParams (toData role) (toData ()) (toData scriptContext) of
              That{} -> False
              _      -> True
