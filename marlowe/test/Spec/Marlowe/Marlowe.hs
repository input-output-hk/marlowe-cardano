{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Spec.Marlowe.Marlowe
    ( prop_noFalsePositives
    , tests
    , prop_showWorksForContracts
    , prop_contractJsonLoops
    , prop_marloweParamsJsonLoops
    , prop_intervalErrorJsonLoops
    )
    where

import qualified Codec.Serialise as Serialise
import Control.Arrow (Arrow ((***)))
import Control.Exception (SomeException, catch)
import Data.Aeson (decode, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import GHC.IO (unsafePerformIO)
import Language.Haskell.Interpreter (Extension (OverloadedStrings), MonadInterpreter, OptionVal ((:=)), as, interpret,
                                     languageExtensions, runInterpreter, set, setImports)
import qualified Language.Marlowe as M
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import Language.Marlowe.Client (defaultMarloweParams, marloweParams)
import Language.Marlowe.Core.V1.Semantics (MarloweParams (MarloweParams),
                                           TransactionInput (TransactionInput, txInputs, txInterval),
                                           TransactionOutput (TransactionOutput, txOutState), computeTransaction,
                                           evalValue)
import Language.Marlowe.Core.V1.Semantics.Types (Action (Choice, Deposit), Bound (Bound), Case (Case),
                                                 ChoiceId (ChoiceId), Contract (Close, If, Pay, When),
                                                 Environment (Environment),
                                                 IntervalError (IntervalInPastError, InvalidInterval),
                                                 Observation (ValueGE), Party (Role), Payee (Account, Party),
                                                 State (State, accounts, boundValues, choices, minTime), Token (Token),
                                                 Value (AddValue, Constant, DivValue, MulValue, NegValue, SubValue, UseValue),
                                                 ValueId (ValueId), emptyState)
import Language.Marlowe.Scripts (smallTypedValidator, smallUntypedValidator)
import Language.Marlowe.Util (ada, extractNonMerkleizedContractRoles)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as TS
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol), POSIXTime (POSIXTime), ValidatorHash (ValidatorHash),
                             toBuiltin)
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.Ratio as P
import Spec.Marlowe.Common (alicePk, amount, contractGen, pangramContract, shrinkContract, valueGen)
import Test.QuickCheck (Gen, arbitrary, counterexample, forAll, forAllShrink, property, suchThat, tabulate,
                        withMaxSuccess, (.&&.), (=/=), (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck (Property, testProperty)

tests :: TestTree
tests = testGroup "Semantics"
  [ testCase "Contracts with different creators have different hashes" uniqueContractHash
  , testCase "Token Show instance respects HEX and Unicode" tokenShowTest
  , testCase "Pangram Contract serializes into valid JSON" pangramContractSerialization
  , testCase "State serializes into valid JSON" stateSerialization
  , testCase "Input serializes into valid JSON" inputSerialization
  , testGroup "Validator size is reasonable"
      [ testCase "Typed validator size" typedValidatorSize
      , testCase "Untyped validator size" untypedValidatorSize
      ]
  , testCase "Mul analysis" mulAnalysisTest
  , testCase "Div analysis" divAnalysisTest
  , testCase "Div tests" divTest
  , testCase "Transfers between accounts work" transferBetweenAccountsTest
  , testCase "extractContractRoles" extractContractRolesTest
  , testProperty "Value equality is reflexive, symmetric, and transitive" checkEqValue
  , testProperty "Value double negation" doubleNegation
  , testProperty "Values form abelian group" valuesFormAbelianGroup
  , testProperty "Values can be serialized to JSON" valueSerialization
  , testProperty "Multiply by zero" mulTest
  , testProperty "Divide zero and by zero" divZeroTest
  , testProperty "DivValue rounding" divisionRoundingTest
-- PAB tests
--  , zeroCouponBondTest
--  , merkleizedZeroCouponBondTest
--  , errorHandlingTest
--  , trustFundTest
  ]
--
-- alicePk ::
-- alicePk = M.PK "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
-- alice, bob :: Wallet
-- alice = w1
-- bob = w2
--
-- reqId :: UUID
-- reqId = UUID.nil
--
--
-- walletPubKeyHash :: Wallet -> PubKeyHash
-- walletPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash
--
-- walletAddress :: Wallet -> AddressInEra ShelleyEra
-- walletAddress wallet = makeShelleyAddressInEra (Shelley.Testnet $ Shelley.NetworkMagic 0) paymentCredential Shelley.NoStakeAddress
--     where
--     paymentCredential = case deserialiseFromRawBytes (AsHash AsPaymentKey) $ P.fromBuiltin $ getPubKeyHash $ walletPubKeyHash wallet of
--         Nothing   -> error "Failed to deserialize pub key hash"
--         Just hash -> Shelley.PaymentCredentialByKey hash
-- zeroCouponBondTest :: TestTree
-- zeroCouponBondTest = checkPredicateOptions defaultCheckOptions "Zero Coupon Bond Contract"
--     (assertNoFailedTransactions
--     -- T..&&. emulatorLog (const False) ""
--     T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag alice) (const True) "contract should close"
--     T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag bob) (const True) "contract should close"
--     T..&&. walletFundsChange alice (lovelaceValueOf 15_000_000)
--     T..&&. walletFundsChange bob (lovelaceValueOf (-15_000_000))
--     T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag alice) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
--     T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag bob) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
--     ) $ do
--     -- Init a contract
--     let alicePk = PK (walletPubKeyHash alice)
--         bobPk = PK (walletPubKeyHash bob)
--
--     let params = defaultMarloweParams
--
--     slotCfg <- Trace.getSlotConfig
--     let seconds = secondsSinceShelley slotCfg
--
--     let zeroCouponBond = When [ Case
--             (Deposit alicePk alicePk ada (Constant 75_000_000))
--             (Pay alicePk (Party bobPk) ada (Constant 75_000_000)
--                 (When
--                     [ Case (Deposit alicePk bobPk ada (Constant 90_000_000)) Close] (seconds 200) Close
--                 ))] (seconds 100) Close
--
--     bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
--     aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
--
--     Trace.callEndpoint @"create" aliceHdl (reqId, AssocMap.empty, zeroCouponBond)
--     Trace.waitNSlots 4
--
--     Trace.callEndpoint @"apply-inputs" aliceHdl (reqId, params, Nothing, [ClientInput $ IDeposit alicePk alicePk ada 75_000_000])
--     Trace.waitNSlots 4
--
--     Trace.callEndpoint @"apply-inputs" bobHdl (reqId, params, Nothing, [ClientInput $ IDeposit alicePk bobPk ada 90_000_000])
--     void $ Trace.waitNSlots 4
--
--     Trace.callEndpoint @"close" aliceHdl reqId
--     Trace.callEndpoint @"close" bobHdl reqId
--     void $ Trace.waitNSlots 2

-- merkleizedZeroCouponBondTest :: TestTree
-- merkleizedZeroCouponBondTest = checkPredicateOptions defaultCheckOptions "Merkleized Zero Coupon Bond Contract"
--     (assertNoFailedTransactions
--     -- T..&&. emulatorLog (const False) ""
--     T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag alice) (const True) "contract should close"
--     T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag bob) (const True) "contract should close"
--     T..&&. walletFundsChange alice (lovelaceValueOf 15_000_000)
--     T..&&. walletFundsChange bob (lovelaceValueOf (-15_000_000))
--     T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag alice) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
--     T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag bob) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
--     ) $ do
--     -- Init a contract
--     let alicePk = PK (walletPubKeyHash alice)
--         bobPk = PK (walletPubKeyHash bob)
--
--     let params = defaultMarloweParams
--
--     slotCfg <- Trace.getSlotConfig
--     let seconds = secondsSinceShelley slotCfg
--
--     let zeroCouponBondStage1 = When [ merkleizedCase (Deposit alicePk alicePk ada (Constant 75_000_000))
--                                                      zeroCouponBondStage2
--                                     ] (seconds 100) Close
--         zeroCouponBondStage2 = Pay alicePk (Party bobPk) ada (Constant 75_000_000)
--                                   (When [ merkleizedCase (Deposit alicePk bobPk ada (Constant 90_000_000))
--                                                          zeroCouponBondStage3
--                                         ] (seconds 200) Close)
--         zeroCouponBondStage3 = Close
--
--     bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
--     aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
--
--     Trace.callEndpoint @"create" aliceHdl (reqId, AssocMap.empty, zeroCouponBondStage1)
--     Trace.waitNSlots 2
--
--     Trace.callEndpoint @"apply-inputs" aliceHdl (reqId, params, Nothing,
--                                                  [ClientMerkleizedInput (IDeposit alicePk alicePk ada 75_000_000) zeroCouponBondStage2])
--     Trace.waitNSlots 4
--
--     Trace.callEndpoint @"apply-inputs" bobHdl (reqId, params, Nothing,
--                                                [ClientMerkleizedInput (IDeposit alicePk bobPk ada 90_000_000) zeroCouponBondStage3])
--     void $ Trace.waitNSlots 4
--
--     Trace.callEndpoint @"close" aliceHdl reqId
--     Trace.callEndpoint @"close" bobHdl reqId
--     void $ Trace.waitNSlots 2
--
--
-- errorHandlingTest :: TestTree
-- errorHandlingTest = checkPredicateOptions defaultCheckOptions "Error handling"
--     (assertAccumState marlowePlutusContract (Trace.walletInstanceTag alice)
--     (\case (Just (EndpointException _ "apply-inputs" _)) -> True
--            _                                             -> False
--     ) "should be fail with EndpointException"
--     ) $ do
--     -- Init a contract
--     let alicePk = PK (walletPubKeyHash alice)
--         bobPk = PK (walletPubKeyHash bob)
--
--     let params = defaultMarloweParams
--
--     slotCfg <- Trace.getSlotConfig
--     let seconds = secondsSinceShelley slotCfg
--
--     let zeroCouponBond = When [ Case
--             (Deposit alicePk alicePk ada (Constant 75_000_000))
--             (Pay alicePk (Party bobPk) ada (Constant 75_000_000)
--                 (When
--                     [ Case (Deposit alicePk bobPk ada (Constant 90_000_000)) Close] (seconds 200) Close
--                 ))] (seconds 100) Close
--
--     bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
--     aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
--
--     Trace.callEndpoint @"create" aliceHdl (reqId, AssocMap.empty, zeroCouponBond)
--     Trace.waitNSlots 2
--
--     Trace.callEndpoint @"apply-inputs" aliceHdl (reqId, params, Nothing, [ClientInput $ IDeposit alicePk alicePk ada 90_000_000])
--     Trace.waitNSlots 2
--     pure ()
--
--
-- minAda :: Integer
-- minAda = 2_000_000
--
--
-- trustFundTest :: TestTree
-- trustFundTest = checkPredicateOptions defaultCheckOptions "Trust Fund Contract"
--     (assertNoFailedTransactions
--     -- T..&&. emulatorLog (const False) ""
--     T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag alice) "contract should not have any errors"
--     T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag bob) "contract should not have any errors"
--     T..&&. walletFundsChange alice (lovelaceValueOf (-minAda-25_600_000) <> Val.singleton (rolesCurrency params) "alice" 1)
--     T..&&. walletFundsChange bob (lovelaceValueOf (minAda+25_600_000) <> Val.singleton (rolesCurrency params) "bob" 1)
--     -- TODO Commented out because the new chain index does not allow to fetch
--     -- all transactions that modified an address. Need to find an alternative
--     -- way.
--     --T..&&. assertAccumState marloweFollowContract "bob follow"
--     --    (\state@ContractHistory{chParams, chHistory} ->
--     --        case chParams of
--     --            First (Just (mp, MarloweData{marloweContract})) -> mp == params && marloweContract == contract
--     --            _                                               -> False) "follower contract state"
--     --        --mp MarloweData{marloweContract} history
--     --        -- chParams == (_ params) && chParams == (_ contract))
--     ) $ do
--
--     -- Init a contract
--     let alicePkh = walletAddress alice
--         bobPkh = walletAddress bob
--     bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
--     aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
--     bobCompanionHdl <- Trace.activateContract bob marloweCompanionContract "bob companion"
--     bobFollowHdl <- Trace.activateContract bob marloweFollowContract "bob follow"
--
--     slotCfg <- Trace.getSlotConfig
--     let seconds = secondsSinceShelley slotCfg
--
--     let contract = When [
--             Case (Choice chId [Bound 10 90_000_000])
--                 (When [Case
--                     (Deposit "alice" "alice" ada (ChoiceValue chId))
--                         (When [Case (Notify (TimeIntervalStart `ValueGE` Constant 15))
--                             (Pay "alice" (Party "bob") ada
--                                 (ChoiceValue chId) Close)]
--                         (seconds 40) Close)
--                     ] (seconds 30) Close)
--             ] (seconds 20) Close
--
--     Trace.callEndpoint @"create" aliceHdl
--         (reqId, AssocMap.fromList [("alice", alicePkh), ("bob", bobPkh)],
--         contract)
--     Trace.waitNSlots 5
--     CompanionState r <- _observableState . instContractState <$> Trace.getContractState bobCompanionHdl
--     case Map.toList r of
--         [] -> pure ()
--         (pms, _) : _ -> do
--
--             Trace.callEndpoint @"apply-inputs" aliceHdl (reqId, pms, Nothing,
--                 [ ClientInput $ IChoice chId 25_600_000
--                 , ClientInput $ IDeposit "alice" "alice" ada 25_600_000
--                 ])
--             Trace.waitNSlots 17
--
--             -- get contract's history and start following our contract
--             Trace.callEndpoint @"follow" bobFollowHdl pms
--             Trace.waitNSlots 4
--
--             Trace.callEndpoint @"apply-inputs" bobHdl (reqId, pms, Nothing, [ClientInput INotify])
--
--             Trace.waitNSlots 4
--             Trace.callEndpoint @"redeem" bobHdl (reqId, pms, "bob", bobPkh)
--             Trace.waitNSlots 4
--             Trace.callEndpoint @"redeem" aliceHdl (reqId, pms, "alice", alicePkh)
--             void $ Trace.waitNSlots 5
--     where
--         alicePk = PK $ walletPubKeyHash alice
--         bobPk = PK $ walletPubKeyHash bob
--         chId = ChoiceId "1" alicePk
--
--         roles = Set.fromList ["alice", "bob"]
--
--         (params, _ :: TxConstraints MarloweInput MarloweData, _) =
--             let con = setupMarloweParams @MarloweSchema @MarloweError
--                         (AssocMap.fromList [("alice", walletAddress alice), ("bob", walletAddress bob)])
--                         roles
--                 fld = Folds.instanceOutcome con (Trace.walletInstanceTag alice)
--                 getOutcome (Done a) = a
--                 getOutcome e        = error $ "not finished: " <> show e
--             in either (error . show) (getOutcome . S.fst')
--                     $ run
--                     $ runError @Folds.EmulatorFoldErr
--                     $ foldEmulatorStreamM fld
--                     $ Trace.runEmulatorStream def
--                     $ do
--                         void $ Trace.activateContractWallet alice (void con)
--                         Trace.waitNSlots 10
--


uniqueContractHash :: IO ()
uniqueContractHash = do
    let hash1 = TS.validatorHash $ smallTypedValidator (marloweParams "11")
    let hash2 = TS.validatorHash $ smallTypedValidator (marloweParams "22")
    let hash3 = TS.validatorHash $ smallTypedValidator (marloweParams "22")
    assertBool "Hashes must be different" (hash1 /= hash2)
    assertBool "Hashes must be same" (hash2 == hash3)

typedValidatorSize :: IO ()
typedValidatorSize = do
    let validator = Scripts.validatorScript $ smallTypedValidator defaultMarloweParams
    let vsize = SBS.length. SBS.toShort . LB.toStrict $ Serialise.serialise validator
    assertBool ("smallTypedValidator is too large " <> show vsize) (vsize < 17200)

untypedValidatorSize :: IO ()
untypedValidatorSize = do
    let validator = Scripts.validatorScript $ smallUntypedValidator defaultMarloweParams
    let vsize = SBS.length. SBS.toShort . LB.toStrict $ Serialise.serialise validator
    assertBool ("smallUntypedValidator is too large " <> show vsize) (vsize < 15200)

extractContractRolesTest :: IO ()
extractContractRolesTest = do
    extractNonMerkleizedContractRoles Close @=? mempty
    extractNonMerkleizedContractRoles
        (Pay (Role "Alice") (Party (Role "Bob")) ada (Constant 1) Close)
            @=? Set.fromList ["Alice", "Bob"]
    extractNonMerkleizedContractRoles
        (When [Case (Deposit (Role "Bob") (Role "Alice") ada (Constant 10)) Close] 10 Close)
            @=? Set.fromList ["Alice", "Bob"]
    extractNonMerkleizedContractRoles
        (When [Case (Choice (ChoiceId "test" (Role "Alice")) [Bound 0 1]) Close] 10 Close)
            @=? Set.fromList ["Alice"]


checkEqValue :: Property
checkEqValue = property $ do
    let gen = do
            a <- valueGen
            b <- valueGen
            c <- valueGen
            return (a, b, c)
    forAll gen $ \(a, b, c) ->
        (a P.== a) -- reflective
            .&&. ((a P.== b) == (b P.== a)) -- symmetric
            --   (a == b && b == c) => (a == c)
            .&&. (not (a P.== b && b P.== c) || a P.== c) -- transitive


doubleNegation :: Property
doubleNegation = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll valueGen $ \a -> eval (NegValue (NegValue a)) === eval a


valuesFormAbelianGroup :: Property
valuesFormAbelianGroup = property $ do
    let gen = do
            a <- valueGen
            b <- valueGen
            c <- valueGen
            return (a, b, c)
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll gen $ \(a, b, c) ->
        -- associativity of addition
        eval (AddValue (AddValue a b) c) === eval (AddValue a (AddValue b c)) .&&.
        -- commutativity of addition
        eval (AddValue a b) === eval (AddValue b a) .&&.
        -- additive identity
        eval (AddValue a (Constant 0)) === eval a .&&.
        -- additive inverse
        eval (AddValue a (NegValue a)) === 0 .&&.
        -- substraction works
        eval (SubValue (AddValue a b) b) === eval a


divisionRoundingTest :: Property
divisionRoundingTest = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    -- test half-even rounding
    let gen = do
            n <- amount
            d <- suchThat amount (/= 0)
            return (n, d)
    forAll gen $ \(n, d) -> eval (DivValue (Constant n) (Constant d)) === roundToZero (n M.% d)
    where
      roundToZero = P.truncate


mulTest :: Property
mulTest = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll valueGen $ \a ->
        eval (MulValue (Constant 0) a) === 0


divZeroTest :: Property
divZeroTest = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll valueGen $ \a ->
        eval (DivValue (Constant 0) a) === 0 .&&.
        eval (DivValue a (Constant 0)) === 0


valueSerialization :: Property
valueSerialization = property $
    forAll valueGen $ \a ->
        let decoded :: Maybe (Value Observation)
            decoded = decode $ encode a
        in Just a === decoded


mulAnalysisTest :: IO ()
mulAnalysisTest = do
    let muliply = foldl (\a _ -> MulValue (UseValue $ ValueId "a") a) (Constant 1) ([1..100] :: [Int])
        contract = If (muliply `M.ValueGE` Constant 10000) Close (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
    result <- warningsTrace contract
--  print result
    assertBool "Analysis ok" $ isRight result


transferBetweenAccountsTest :: IO ()
transferBetweenAccountsTest = do
    let state = State
            { accounts = AssocMap.fromList [((Role "alice", Token "" ""), 100)]
            , choices  = AssocMap.empty
            , boundValues = AssocMap.empty
            , minTime = 10 }
    let contract = Pay "alice" (Account "bob") (Token "" "") (Constant 100) (When [] 100 Close)
    let txInput = TransactionInput {
                    txInterval = (20, 30),
                    txInputs = [] }
    case computeTransaction txInput state contract of
        TransactionOutput {txOutState = State{accounts}} -> do
            assertBool "Accounts check" $ accounts == AssocMap.fromList [(("bob",Token "" ""), 100)]
        e -> fail $ show e


divAnalysisTest :: IO ()
divAnalysisTest = do
    let
        contract n d = If (DivValue (Constant n) (Constant d) `ValueGE` Constant 5)
            Close
            (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
    result <- warningsTrace (contract 11 2)
    assertBool "Analysis ok" $ isRight result && either (const False) isNothing result
    result' <- warningsTrace (contract 9 2)
    assertBool "Analysis ok" $ isRight result' && either (const False) isJust result'

    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    eval (DivValue (Constant 0) (Constant 2)) @=? 0
    eval (DivValue (Constant 1) (Constant 0)) @=? 0
    eval (DivValue (Constant 5) (Constant 2)) @=? 2
    eval (DivValue (Constant (-5)) (Constant 2)) @=? -2
    eval (DivValue (Constant 7) (Constant 2)) @=? 3
    eval (DivValue (Constant (-7)) (Constant 2)) @=? -3


divTest :: IO ()
divTest = do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    eval (DivValue (Constant 0) (Constant 2)) @=? 0
    eval (DivValue (Constant 1) (Constant 0)) @=? 0
    eval (DivValue (Constant 5) (Constant 2)) @=? 2
    eval (DivValue (Constant (-5)) (Constant 2)) @=? -2
    eval (DivValue (Constant 7) (Constant 2)) @=? 3
    eval (DivValue (Constant (-7)) (Constant 2)) @=? -3



pangramContractSerialization :: IO ()
pangramContractSerialization = do
    let json = toStrict (encodeToLazyText pangramContract)
    -- uncomment to generate json after updating pangramContract
--  T.putStrLn json
    Just pangramContract @=? (decode $ encode pangramContract)
--  T.putStrLn . T.pack . show $ pangramContract
    contract <- readFile "test/contract.json"
    let decoded :: Maybe M.Contract
        decoded = decode (fromString contract)
    case decoded of
        Just cont -> cont @=? pangramContract
        _         -> assertFailure "Nope"


tokenShowTest :: IO ()
tokenShowTest = do
    -- SCP-834, CurrencySymbol is HEX encoded ByteString,
    -- and TokenSymbol as UTF8 encoded Unicode string
    let actual :: M.Value M.Observation
        actual = M.AvailableMoney (M.Role "alice") (M.Token "00010afF" "ÚSD©")

    show actual @=? "AvailableMoney \"alice\" (Token \"00010aff\" \"ÚSD©\")"


inputSerialization :: IO ()
inputSerialization = do
    state <- readFile "test/input.json"
    let decoded :: Either String [M.Input]
        decoded = eitherDecode (fromString state)
    case decoded of
        Right input ->
            case eitherDecode $ encode input of
                Right input' -> assertBool "Should be equal" (input == input')
                Left e       -> assertFailure $ "Could not decode encoded input: " <> e
        Left e -> assertFailure $ "Could not decode test/input.json: " <> e

stateSerialization :: IO ()
stateSerialization = do
    state <- readFile "test/state.json"
    let decoded :: Maybe M.State
        decoded = decode (fromString state)
    case decoded of
        Just st ->
            case decode $ encode st of
                Just st' -> assertBool "Should be equal" (st P.== st')
                Nothing  -> assertFailure "Nope"
        Nothing -> assertFailure "Nope"

prop_showWorksForContracts :: Property
prop_showWorksForContracts = forAllShrink contractGen shrinkContract showWorksForContract


showWorksForContract :: Contract -> Property
showWorksForContract contract = unsafePerformIO $ do
  res <- runInterpreter $ setImports ["Language.Marlowe"]
                        >> set [ languageExtensions := [ OverloadedStrings ] ]
                        >> interpretContractString (show contract)
  return (case res of
            Right x  -> x === contract
            Left err -> counterexample (show err) False)


interpretContractString :: MonadInterpreter m => String -> m Contract
interpretContractString contractStr = interpret contractStr (as :: Contract)


noFalsePositivesForContract :: Contract -> Property
noFalsePositivesForContract cont =
  unsafePerformIO (do res <- catch (first Right <$> warningsTrace cont)
                                   (\exc -> return $ Left (Left (exc :: SomeException)))
                      return (case res of
                                Left err -> counterexample (show err) False
                                Right answer ->
                                   tabulate "Has counterexample" [show (isJust answer)]
                                   (case answer of
                                      Nothing ->
                                         tabulate "Is empty contract" [show (cont == Close)]
                                                  True
                                      Just (is, li, warns) ->
                                         counterexample ("Trace: " ++ show (is, li)) $
                                         tabulate "Number of warnings" [show (length warns)]
                                                  (warns =/= []))))


prop_noFalsePositives :: Property
prop_noFalsePositives = forAllShrink contractGen shrinkContract noFalsePositivesForContract

contractJsonLoops :: Contract -> Property
contractJsonLoops cont = decode (encode cont) === Just cont

prop_contractJsonLoops :: Property
prop_contractJsonLoops = withMaxSuccess 1000 $ forAllShrink contractGen shrinkContract contractJsonLoops

marloweParamsJsonLoops :: MarloweParams -> Property
marloweParamsJsonLoops mp = decode (encode mp) === Just mp

prop_marloweParamsJsonLoops :: Property
prop_marloweParamsJsonLoops = withMaxSuccess 1000 $ forAll gen marloweParamsJsonLoops
  where
    gen = do
      b <- toBuiltin <$> (arbitrary :: Gen ByteString)
      c <- toBuiltin <$> (arbitrary :: Gen ByteString)
      return $ MarloweParams (ValidatorHash b) (CurrencySymbol c)


intervalErrorJsonLoops :: IntervalError -> Property
intervalErrorJsonLoops ie = decode (encode ie) === Just ie

prop_intervalErrorJsonLoops :: Property
prop_intervalErrorJsonLoops = withMaxSuccess 1000 $ forAll gen intervalErrorJsonLoops
  where
    gen = do
      b <- arbitrary
      if b
      then do
        t <- (POSIXTime *** POSIXTime) <$> arbitrary
        return $ InvalidInterval t
      else do
        s <- POSIXTime <$> arbitrary
        t <- (POSIXTime *** POSIXTime) <$> arbitrary
        return $ IntervalInPastError s t

