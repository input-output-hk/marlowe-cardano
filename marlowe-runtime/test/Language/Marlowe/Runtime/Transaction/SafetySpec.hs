{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Transaction.SafetySpec where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Shelley
import Data.Foldable (for_)
import Data.List (isInfixOf, nub)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as M (empty, fromList, keys, lookup, mapKeys, singleton, toList, (!))
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..))
import Language.Marlowe.Core.V1.Merkle as V1 (MerkleizedContract (..), deepMerkleize, merkleizedContract)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import qualified Language.Marlowe.Runtime.Cardano.Api as Chain (
  assetsToCardanoValue,
  fromCardanoAddressInEra,
  toCardanoAddressAny,
  toCardanoDatumHash,
  toCardanoPaymentCredential,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (
  AssetId (..),
  Assets (..),
  Credential (..),
  DatumHash (..),
  PolicyId (..),
  TokenName (..),
  Tokens (..),
 )
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (MarloweV1))
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..), MarloweScripts (..), getCurrentScripts)
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusTokenName)
import Language.Marlowe.Runtime.Transaction.Api (Mint (..), RoleTokensConfig (..))
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  MinAdaProvider (..),
  RolesPolicyId (RolesPolicyId),
  initialMarloweDatum,
 )
import Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec ()
import Language.Marlowe.Runtime.Transaction.Constraints (
  HelperScriptInfo (..),
  HelperScriptState (HelperScriptState),
  HelpersContext (..),
  MarloweContext (..),
  mkTxOutValue,
 )
import Language.Marlowe.Runtime.Transaction.ConstraintsSpec (protocolTestnet)
import Language.Marlowe.Runtime.Transaction.Query.Helper (getHelperInfos)
import Language.Marlowe.Runtime.Transaction.Safety (
  ThreadTokenAssetId (..),
  checkContract,
  checkTransactions,
  minAdaUpperBound,
  mkAdjustMinUTxO,
  mkLockedRolesContext,
  mockLockedRolesContext,
  noContinuations,
 )
import qualified PlutusLedgerApi.V1.Value as PLA
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V2 as Plutus (
  Address (..),
  Credential (..),
  CurrencySymbol (..),
  DatumHash (..),
  TokenName (..),
 )
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Builtins as Plutus (fromBuiltin, lengthOfByteString, toBuiltin)
import Spec.Marlowe.Reference (readReferenceContracts)
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, discard, elements, generate, sublistOf, suchThat, (===), (==>))
import Test.QuickCheck.Arbitrary (arbitrary)

type TestEra = Cardano.BabbageEra

testEra :: Cardano.CardanoEra TestEra
testEra = Cardano.BabbageEra

shelleyBasedEraTest :: Cardano.ShelleyBasedEra TestEra
shelleyBasedEraTest = Cardano.ShelleyBasedEraBabbage

babbageEraOnwardsTest :: Cardano.BabbageEraOnwards TestEra
babbageEraOnwardsTest = Cardano.BabbageEraOnwardsBabbage

alonzoEraOnwardsTest :: Cardano.AlonzoEraOnwards TestEra
alonzoEraOnwardsTest = Cardano.AlonzoEraOnwardsBabbage

maryEraOnwardsTest :: Cardano.MaryEraOnwards TestEra
maryEraOnwardsTest = Cardano.MaryEraOnwardsBabbage

spec :: Spec
spec =
  do
    let testnet = Cardano.Testnet $ Cardano.NetworkMagic 1
        version = MarloweV1
        adjustMinUtxo = mkAdjustMinUTxO babbageEraOnwardsTest protocolTestnet MarloweV1
        emptyHelpersContext = HelpersContext M.empty "" M.empty
        emptyLockedRolesContext = mkLockedRolesContext emptyHelpersContext
        continuations = noContinuations version
        party = V1.Role "x"
        payee = V1.Party party
        payToken token = V1.Pay party payee token $ V1.Constant 1
        payRole role = V1.Pay (V1.Role role) (V1.Party $ V1.Role role) (V1.Token "" "") $ V1.Constant 1
        same x y = nub x == x && nub y == y && not (any (`notElem` x) y) && not (any (`notElem` y) x)

    describe "minAdaUpperBound" $
      do
        let
        prop "At least Cardano.Api" $ \(address, hash, assets@(Chain.Assets _ tokens')) ->
          do
            let value = fromJust $ Chain.assetsToCardanoValue assets
                toToken (Chain.AssetId (Chain.PolicyId p) (Chain.TokenName n)) =
                  V1.Token
                    (Plutus.CurrencySymbol $ Plutus.toBuiltin p)
                    (Plutus.TokenName $ Plutus.toBuiltin n)
                tokens = fmap toToken . M.keys $ Chain.unTokens tokens'
                expected =
                  Cardano.calculateMinimumUTxO
                    shelleyBasedEraTest
                    ( Cardano.TxOut
                        (Cardano.anyAddressInShelleyBasedEra shelleyBasedEraTest . fromJust $ Chain.toCardanoAddressAny address)
                        (mkTxOutValue maryEraOnwardsTest value)
                        (Cardano.TxOutDatumHash alonzoEraOnwardsTest . fromJust $ Chain.toCardanoDatumHash hash)
                        Shelley.ReferenceScriptNone
                    )
                    $ Shelley.unLedgerProtocolParameters protocolTestnet
                    :: Cardano.Lovelace
                contract = foldr payToken V1.Close tokens -- The tokens just need to appear somewhere in the contract.
                actual =
                  fromJust $
                    minAdaUpperBound
                      babbageEraOnwardsTest
                      protocolTestnet
                      version
                      (V1.emptyState 0)
                      contract
                      continuations
                    :: Cardano.Lovelace
            counterexample ("Expected minUTxO = " <> show expected) $
              counterexample ("Actual minUTxO = " <> show actual) $
                actual >= expected

    describe "checkContract" $
      do
        prop "Contract without roles" $ \roleTokensConfig ->
          let contract = V1.Close
              actual = checkContract testnet roleTokensConfig version contract Nothing continuations
           in counterexample ("Contract = " <> show contract) $
                case roleTokensConfig of
                  RoleTokensNone -> actual === []
                  _ -> actual === [ContractHasNoRoles]
        prop "Contract with roles from minting" $ \mint ->
          let roles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (NEMap.toMap $ unMint mint)
              contract = foldr payRole V1.Close roles
              actual = checkContract testnet (RoleTokensMint mint) version contract Nothing continuations
           in counterexample ("Contract = " <> show contract) $
                actual === mempty
        prop "Contract with roles missing from minting" $ \roleTokensConfig extra ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              all (\t -> not (NEMap.member (fromPlutusTokenName t) $ unMint mint)) extra ==>
                let roles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (NEMap.toMap $ unMint mint)
                    contract = foldr payRole V1.Close $ extra <> roles
                    actual = checkContract testnet roleTokensConfig version contract Nothing continuations
                    expected =
                      (MissingRoleToken <$> nub extra)
                        <> [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub extra, Plutus.lengthOfByteString name > 32]
                 in counterexample ("Contract = " <> show contract)
                      . counterexample ("Actual = " <> show actual)
                      . counterexample ("Expected = " <> show expected)
                      $ actual `same` expected
            _ -> discard
        prop "Contract with extra roles for minting" $ \roleTokensConfig ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              do
                let roles' = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (NEMap.toMap $ unMint mint)
                roles <- sublistOf roles' `suchThat` (not . null)
                let extra = filter (`notElem` roles) roles'
                    contract = foldr payRole V1.Close roles
                    actual = checkContract testnet roleTokensConfig version contract Nothing continuations
                    expected = ExtraRoleToken <$> extra
                pure
                  . counterexample ("Contract = " <> show contract)
                  . counterexample ("Actual = " <> show actual)
                  . counterexample ("Expected = " <> show expected)
                  $ actual `same` expected
            _ -> discard
        prop "Contract with role name too long" $ \roles ->
          let contract = foldr payRole V1.Close roles
              actual = checkContract testnet (RoleTokensUsePolicy "" mempty) version contract Nothing continuations
              expected =
                if null roles
                  then [ContractHasNoRoles]
                  else [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub roles, Plutus.lengthOfByteString name > 32]
           in counterexample ("Contract = " <> show contract)
                . counterexample ("Actual = " <> show actual)
                . counterexample ("Expected = " <> show expected)
                $ actual `same` expected
        -- prop "State with role name too long" $ \roles ->
        --   let contract = V1.Close
        --       state =
        --       expected =
        --           [ContractHasNoRoles] <>
        --           [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub roles, Plutus.lengthOfByteString name > 32]
        --       actual = checkContract testnet (RoleTokensUsePolicy "" mempty) version contract Nothing continuations
        --    in counterexample ("Contract = " <> show contract)
        --         . counterexample ("Actual = " <> show actual)
        --         . counterexample ("Expected = " <> show expected)
        --         $ actual `same` expected
        prop "Contract with illegal token" $ \tokens ->
          let contract = foldr payToken V1.Close tokens
              actual = checkContract testnet (RoleTokensUsePolicy "" mempty) version contract Nothing continuations
              expected =
                if contract == V1.Close
                  then [ContractHasNoRoles]
                  else
                    nub
                      [ if badToken
                        then InvalidToken token
                        else
                          if badCurrency
                            then InvalidCurrencySymbol symbol
                            else TokenNameTooLong name
                      | token@(V1.Token symbol@(Plutus.CurrencySymbol symbol') name@(Plutus.TokenName name')) <- nub tokens
                      , let ada = symbol' == "" && name' == ""
                      , let badToken = symbol' == "" && name' /= ""
                      , let badCurrency = Plutus.lengthOfByteString symbol' /= 28
                      , let badName = Plutus.lengthOfByteString name' > 32
                      , not ada
                      , badToken || badCurrency || badName
                      ]
           in counterexample ("Contract = " <> show contract)
                . counterexample ("Actual = " <> show actual)
                . counterexample ("Expected = " <> show expected)
                $ actual `same` expected
        prop "Contract with missing continuation" $ \contract ->
          do
            let V1.MerkleizedContract{..} = V1.merkleizedContract $ V1.deepMerkleize contract
                toChainDatumHash (Plutus.DatumHash x) = Chain.DatumHash $ Plutus.fromBuiltin x
                continuations' = M.toList $ M.mapKeys toChainDatumHash mcContinuations
            missing <- if null continuations' then pure mempty else pure <$> elements continuations'
            let remaining = filter (`notElem` missing) continuations'
                relevant ContractHasNoRoles = False
                relevant (RoleNameTooLong _) = False
                relevant (InvalidCurrencySymbol _) = False
                relevant (TokenNameTooLong _) = False
                relevant _ = True
                actual =
                  filter relevant $
                    checkContract testnet (RoleTokensUsePolicy "" mempty) version mcContract Nothing (M.fromList remaining)
                expected = MissingContinuation . Plutus.DatumHash . Plutus.toBuiltin . Chain.unDatumHash . fst <$> missing
            pure
              . counterexample ("Contract = " <> show mcContract)
              . counterexample ("Missing = " <> show missing)
              . counterexample ("Remaining = " <> show remaining)
              . counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected
        prop "Contract with inconsistent networks" $ \address ->
          do
            let contract =
                  V1.When
                    [V1.Case (V1.Deposit (V1.Address True address) (V1.Address False address) (V1.Token "" "") (V1.Constant 1)) V1.Close]
                    0
                    V1.Close
                actual = checkContract testnet RoleTokensNone version contract Nothing mempty
                expected = [InconsistentNetworks, WrongNetwork]
            counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected
        prop "Contract on wrong network" $ \address ->
          do
            let contract = V1.When [V1.Case (V1.Choice (V1.ChoiceId "Choice" $ V1.Address V1.mainnet address) []) V1.Close] 0 V1.Close
                actual = checkContract testnet RoleTokensNone version contract Nothing mempty
                expected = [WrongNetwork]
            counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected
        prop
          "Contract with bad address"
          do
            let address =
                  Plutus.Address
                    (Plutus.PubKeyCredential "0000000000000000000000000000000000000000000000000000000000000000") -- The hash is too long.
                    Nothing
                contract = V1.When [V1.Case (V1.Choice (V1.ChoiceId "Choice" $ V1.Address False address) []) V1.Close] 0 V1.Close
                actual = checkContract testnet RoleTokensNone version contract Nothing mempty
                expected = [IllegalAddress address]
            counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected

    describe "checkTransactions" do
      referenceContracts <- runIO readReferenceContracts
      let networkId = Cardano.Testnet $ Cardano.NetworkMagic 1
          MarloweScripts{..} = getCurrentScripts version
          stakeReference = Shelley.NoStakeAddress
          marloweContext =
            MarloweContext
              { scriptOutput = Nothing
              , marloweAddress =
                  Chain.fromCardanoAddressInEra testEra
                    . Cardano.AddressInEra (Cardano.ShelleyAddressInEra shelleyBasedEraTest)
                    $ Cardano.makeShelleyAddress
                      networkId
                      (fromJust . Chain.toCardanoPaymentCredential $ Chain.ScriptCredential marloweScript)
                      stakeReference
              , payoutAddress =
                  Chain.fromCardanoAddressInEra testEra
                    . Cardano.AddressInEra (Cardano.ShelleyAddressInEra shelleyBasedEraTest)
                    $ Cardano.makeShelleyAddress
                      networkId
                      (fromJust . Chain.toCardanoPaymentCredential $ Chain.ScriptCredential payoutScript)
                      Cardano.NoStakeAddress
              , marloweScriptUTxO = fromJust $ M.lookup networkId marloweScriptUTxOs
              , payoutScriptUTxO = fromJust $ M.lookup networkId payoutScriptUTxOs
              , marloweScriptHash = marloweScript
              , payoutScriptHash = payoutScript
              }
          overspentOrWarning (TransactionValidationError _ msg) = "The machine terminated part way through evaluation due to overspending the budget." `isInfixOf` msg
          overspentOrWarning (TransactionWarning _ _) = True
          overspentOrWarning _ = False
          overspent (TransactionValidationError _ msg) = "The machine terminated part way through evaluation due to overspending the budget." `isInfixOf` msg
          overspent _ = False
      for_ referenceContracts \(name, contract) -> it ("Passes for reference contract " <> name) do
        (policy, address) <- generate arbitrary
        let minAda =
              fromIntegral $
                maybe 1_500_000 toInteger $
                  minAdaUpperBound
                    ReferenceTxInsScriptsInlineDatumsInBabbageEra
                    protocolTestnet
                    version
                    (V1.emptyState 0)
                    contract
                    mempty
            threadTokenAssetId = ThreadTokenAssetId (Chain.AssetId policy (Chain.TokenName "Thread"))
        datum <-
          either (error "Unable to initialize the state") pure $
            initialMarloweDatum
              contract
              (RolesPolicyId policy)
              adjustMinUtxo
              mempty
              MarloweV1
              (Just threadTokenAssetId)
              minAda
              (MinAdaProvider address)
        actual <-
          checkTransactions
            protocolTestnet
            babbageEraOnwardsTest
            version
            marloweContext
            emptyLockedRolesContext
            address
            datum
            continuations
        case actual of
          -- Overspending or warnings are not a test failures.
          Right errs
            | all overspentOrWarning errs -> pure ()
          -- An ambiguous time interval occurs when the timeouts have non-zero milliseconds are too close for there to be a valid slot for a transaction.
          Left "ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed \"TEAmbiguousTimeIntervalError\")" -> pure ()
          -- All other results are test failures.
          _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
      prop
        "Contract with prohibitive execution cost"
        do
          let minAda = 3_000_000
              policy = "46e79d4fbf0dd6766f8601fdec651ad708af7115fd8f7b5e14b622e5"
              address = "608db2b806ba9e7ae2909ae38afc6c1bce02f5df3e1cb1b06cbc80546f"
              rolesPolicyId = RolesPolicyId policy
              ada = V1.Token PV2.adaSymbol PV2.adaToken
              contract =
                V1.When
                  [ V1.Case (V1.Deposit party party ada $ V1.Constant i) $ V1.Pay party (V1.Party party) ada (V1.Constant i) V1.Close
                  | i <- [1 .. 50]
                  ]
                  1000
                  V1.Close
          datum <-
            either (error "Unable to initialize the state") pure $
              initialMarloweDatum
                contract
                rolesPolicyId
                adjustMinUtxo
                mempty
                MarloweV1
                Nothing
                minAda
                (MinAdaProvider address)

          actual <-
            checkTransactions
              protocolTestnet
              babbageEraOnwardsTest
              version
              marloweContext
              emptyLockedRolesContext
              address
              datum
              continuations
          case actual of
            Right errs
              | all overspent errs -> pure ()
            _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
      prop
        "Contract with prohibitive execution closure cost"
        do
          let adaToken = V1.Token PV2.adaSymbol PV2.adaToken
              -- Eight payouts is enough to overspend the budget
              initialAccounts :: V1.Accounts
              initialAccounts =
                AM.fromList $
                  [ ((accountId, adaToken), 1)
                  | i <- [1 .. 8] :: [Int]
                  , let accountId = V1.Role $ PLA.tokenName . Text.encodeUtf8 . Text.pack . show $ i
                  ]
              policy = "46e79d4fbf0dd6766f8601fdec651ad708af7115fd8f7b5e14b622e5"
              marloweParams = V1.MarloweParams policy
              state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = initialAccounts}
              contract =
                V1.When
                  [ V1.Case
                      (V1.Notify V1.TrueObs)
                      V1.Close
                  ]
                  30_000
                  V1.Close
              datum = V1.MarloweData marloweParams state contract
              address = "608db2b806ba9e7ae2909ae38afc6c1bce02f5df3e1cb1b06cbc80546f"

          actual <-
            checkTransactions
              protocolTestnet
              ReferenceTxInsScriptsInlineDatumsInBabbageEra
              version
              marloweContext
              emptyLockedRolesContext
              address
              datum
              continuations
          case actual of
            Right [] -> expectationFailure "We expected overspending the budget n this case"
            Right errs | all overspentOrWarning errs -> pure ()
            -- All other results are test failures.
            _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
      prop
        "Contract with open roles"
        do
          let address = "608db2b806ba9e7ae2909ae38afc6c1bce02f5df3e1cb1b06cbc80546f"
              helperPolicyId = "46e79d4fbf0dd6766f8601fdec651ad708af7115fd8f7b5e14b622e5"
              currentHelperScripts = getHelperInfos helperScript networkId $ getCurrentScripts MarloweV1
              helperScriptStates = M.singleton "Beneficiary" $ HelperScriptState (currentHelperScripts M.! OpenRoleScript) Nothing
              helpersContext = HelpersContext{..}
              rolesPolicyId = RolesPolicyId helperPolicyId
              threadTokenAssetId = ThreadTokenAssetId (Chain.AssetId helperPolicyId (Chain.TokenName "Thread"))
              lockedRolesContext = mockLockedRolesContext threadTokenAssetId adjustMinUtxo helpersContext
              benefactor = "Benefactor"
              beneficiary = "Beneficiary"
              amount = V1.Constant 8_000_000
              ada = V1.Token "" ""
              contract =
                V1.When
                  [ V1.Case (V1.Deposit (V1.Role benefactor) (V1.Role benefactor) ada amount) $
                      V1.When
                        [ V1.Case
                            (V1.Deposit (V1.Role beneficiary) (V1.Role beneficiary) ada amount)
                            $ V1.Pay (V1.Role benefactor) (V1.Account (V1.Role beneficiary)) ada amount
                            $ V1.When
                              [ V1.Case
                                  (V1.Notify V1.TrueObs)
                                  V1.Close
                              ]
                              30_000
                              V1.Close
                        ]
                        20_000
                        V1.Close
                  ]
                  10_000
                  V1.Close
              minAda = 3_000_000

          datum <-
            either (error "Unable to initialize the state") pure $
              initialMarloweDatum
                contract
                rolesPolicyId
                adjustMinUtxo
                mempty
                MarloweV1
                (Just threadTokenAssetId)
                minAda
                (MinAdaProvider address)
          actual <-
            checkTransactions
              protocolTestnet
              babbageEraOnwardsTest
              version
              marloweContext
              lockedRolesContext
              address
              datum
              continuations
          case actual of
            -- Overspending or warnings are not a test failures.
            Right errs
              | all overspentOrWarning errs -> pure ()
            -- An ambiguous time interval occurs when the timeouts have non-zero milliseconds are too close for there to be a valid slot for a transaction.
            Left "ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed \"TEAmbiguousTimeIntervalError\")" -> pure ()
            -- All other results are test failures.
            _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
