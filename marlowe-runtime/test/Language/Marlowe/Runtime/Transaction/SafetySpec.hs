{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Transaction.SafetySpec where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Shelley
import Control.Category ((<<<))
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (isInfixOf, nub)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as M (Map, empty, fromList, keys, lookup, mapKeys, singleton, toList, (!))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Marlowe.Analysis.Safety.Ledger (checkContinuations)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..))
import qualified Language.Marlowe.Analysis.Safety.Types as S
import Language.Marlowe.Core.V1.Merkle as V1 (MerkleizedContract (..), deepMerkleize, merkleizedContract)
import qualified Language.Marlowe.Core.V1.Plate as V1
import Language.Marlowe.Core.V1.Semantics (MarloweData (..), MarloweParams (rolesCurrency))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoLovelace)
import qualified Language.Marlowe.Runtime.Cardano.Api as Chain (
  assetsToCardanoValue,
  fromCardanoAddressInEra,
  toCardanoAddressAny,
  toCardanoDatumHash,
  toCardanoPaymentCredential,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (
  AssetId (..),
  Assets (..),
  Credential (..),
  DatumHash (..),
  Lovelace (..),
  PolicyId (..),
  TokenName (..),
  Tokens (..),
 )
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (MarloweV1))
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..), MarloweScripts (..), getCurrentScripts)
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusTokenName, toPlutusCurrencySymbol)
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
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
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
        ada = V1.Token PV2.adaSymbol PV2.adaToken
        datumForContract contract rolesCurrency =
          V1.MarloweData
            { marloweParams = V1.MarloweParams{rolesCurrency}
            , marloweContract = contract
            , marloweState = V1.emptyState 0
            }

        mockPolicyId = "00000000000000000000000000000000000000000000000000000000"
        policyIdFromRolesConfig =
          toPlutusCurrencySymbol <<< \case
            RoleTokensNone -> ""
            RoleTokensUsePolicy policyId _ -> policyId
            RoleTokensMint _ -> "00000000000000000000000000000000000000000000000000000000"
        datumForContractAndRolesConfig contract rolesConfig = do
          let rolesCurrency = policyIdFromRolesConfig rolesConfig
          datumForContract contract rolesCurrency

    describe "minAdaUpperBound" $
      do
        prop "At least Cardano.Api" $ \(address, hash, assets@(CS.TxOutAssetsContent (Chain.Assets _ tokens'))) ->
          do
            let value = fromJust $ Chain.assetsToCardanoValue assets
                toToken (Chain.AssetId (Chain.PolicyId p) (Chain.TokenName n)) =
                  V1.Token
                    (Plutus.CurrencySymbol $ Plutus.toBuiltin p)
                    (Plutus.TokenName $ Plutus.toBuiltin n)
                tokens = fmap toToken . M.keys $ Chain.unTokens tokens'
                expected =
                  fromCardanoLovelace
                    ( Cardano.calculateMinimumUTxO
                        shelleyBasedEraTest
                        ( Cardano.TxOut
                            (Cardano.anyAddressInShelleyBasedEra shelleyBasedEraTest . fromJust $ Chain.toCardanoAddressAny address)
                            (mkTxOutValue maryEraOnwardsTest value)
                            (Cardano.TxOutDatumHash alonzoEraOnwardsTest . fromJust $ Chain.toCardanoDatumHash hash)
                            Shelley.ReferenceScriptNone
                        )
                        $ Shelley.unLedgerProtocolParameters protocolTestnet
                        :: Cardano.Lovelace
                    )
                contract = foldr payToken V1.Close tokens -- The tokens just need to appear somewhere in the contract.
                actual =
                  minAdaUpperBound
                    babbageEraOnwardsTest
                    protocolTestnet
                    version
                    (V1.emptyState 0)
                    contract
                    continuations
            counterexample ("Expected minUTxO = " <> show expected) $
              counterexample ("Actual minUTxO = " <> show actual) $ fromMaybe False do
                a <- actual
                pure (a >= expected)

    describe "checkContract" $
      do
        prop "Contract and state without roles" $ \roleTokensConfig ->
          let contract = V1.Close
              datum = datumForContractAndRolesConfig contract roleTokensConfig
              actual = checkContract testnet (Just roleTokensConfig) version datum continuations
           in counterexample ("Contract = " <> show contract) $
                case roleTokensConfig of
                  RoleTokensNone -> actual === []
                  _ -> actual === [ContractHasNoRoles]
        prop "Contract or state with roles from minting" $ \mint doTestContract ->
          let roles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (NEMap.toMap $ unMint mint)
              (contract, datum) =
                if doTestContract
                  then do
                    let cnt = foldr payRole V1.Close roles
                        dtm = datumForContractAndRolesConfig contract (RoleTokensMint mint)
                    (cnt, dtm)
                  else do
                    let cnt = V1.Close
                        accounts = AM.fromList $ roles <&> \role -> ((V1.Role role, V1.Token "" ""), 1)
                        state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts}
                        dtm =
                          V1.MarloweData
                            { marloweParams = V1.MarloweParams{rolesCurrency = policyIdFromRolesConfig (RoleTokensMint mint)}
                            , marloweContract = contract
                            , marloweState = state
                            }
                    (cnt, dtm)
              roleTokensConfig = RoleTokensMint mint
              actual = checkContract testnet (Just roleTokensConfig) version datum continuations
           in counterexample ("Contract = " <> show contract) $
                actual === mempty
        prop "Contract or state with roles missing from minting" $ \roleTokensConfig extra doTestContract ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              all (\t -> not (NEMap.member (fromPlutusTokenName t) $ unMint mint)) extra && not (null extra) ==>
                let roles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (NEMap.toMap $ unMint mint)
                    (contract, datum) =
                      if doTestContract
                        then do
                          let cnt = foldr payRole V1.Close $ extra <> roles
                              dtm = datumForContractAndRolesConfig contract roleTokensConfig
                          (cnt, dtm)
                        else do
                          let cnt = V1.Close
                              accounts = AM.fromList $ (extra <> roles) <&> \role -> ((V1.Role role, V1.Token "" ""), 1)
                              state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts}
                              dtm =
                                V1.MarloweData
                                  { marloweParams = V1.MarloweParams{rolesCurrency = policyIdFromRolesConfig roleTokensConfig}
                                  , marloweContract = contract
                                  , marloweState = state
                                  }
                          (cnt, dtm)
                    actual = checkContract testnet (Just roleTokensConfig) version datum continuations
                    expected =
                      (MissingRoleToken <$> nub extra)
                        <> [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub extra, Plutus.lengthOfByteString name > 32]
                 in counterexample ("Contract = " <> show contract)
                      . counterexample ("Datum = " <> show datum)
                      . counterexample ("Actual = " <> show actual)
                      . counterexample ("Expected = " <> show expected)
                      $ actual `same` expected
            _ -> discard
        prop "Contract or state with extra roles for minting" $ \roleTokensConfig doTestContract ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              do
                let mintedRoles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (NEMap.toMap $ unMint mint)
                contractRoles <- sublistOf mintedRoles `suchThat` (not . null)
                let extraMintedRoles = filter (`notElem` contractRoles) mintedRoles
                when (null extraMintedRoles) discard
                let (contract, datum) =
                      if doTestContract
                        then do
                          let cnt = foldr payRole V1.Close contractRoles
                              dtm = datumForContractAndRolesConfig contract roleTokensConfig
                          (cnt, dtm)
                        else do
                          let cnt = V1.Close
                              accounts = AM.fromList $ contractRoles <&> \role -> ((V1.Role role, V1.Token "" ""), 1)
                              state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts}
                              dtm =
                                V1.MarloweData
                                  { marloweParams = V1.MarloweParams{rolesCurrency = policyIdFromRolesConfig roleTokensConfig}
                                  , marloweContract = contract
                                  , marloweState = state
                                  }
                          (cnt, dtm)
                    actual = checkContract testnet (Just roleTokensConfig) version datum continuations
                    expected = ExtraRoleToken <$> extraMintedRoles
                pure
                  . counterexample ("Contract = " <> show contract)
                  . counterexample ("Datum = " <> show datum)
                  . counterexample ("Actual = " <> show actual)
                  . counterexample ("Expected = " <> show expected)
                  $ actual `same` expected
            _ -> discard
        prop "Contract or state with role name too long" $ \roles doTestContract -> do
          let roleTokensConfig = RoleTokensUsePolicy mockPolicyId mempty
              (contract, datum) =
                if doTestContract
                  then do
                    let cnt = foldr payRole V1.Close roles
                        dtm = datumForContractAndRolesConfig contract roleTokensConfig
                    (cnt, dtm)
                  else do
                    let cnt = V1.Close
                        accounts = AM.fromList $ roles <&> \role -> ((V1.Role role, V1.Token "" ""), 1)
                        state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts}
                        dtm =
                          V1.MarloweData
                            { marloweParams = V1.MarloweParams{rolesCurrency = policyIdFromRolesConfig roleTokensConfig}
                            , marloweContract = contract
                            , marloweState = state
                            }
                    (cnt, dtm)
              actual = checkContract testnet (Just roleTokensConfig) version datum continuations
              expected =
                if null roles
                  then [ContractHasNoRoles]
                  else [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub roles, Plutus.lengthOfByteString name > 32]
          counterexample ("Contract = " <> show contract)
            . counterexample ("Actual = " <> show actual)
            . counterexample ("Expected = " <> show expected)
            $ actual `same` expected
        prop "Marlowe with illegal token" $ \tokens doTestContract -> do
          let (contract, state) =
                if doTestContract
                  then (foldr payToken V1.Close tokens, Nothing)
                  else do
                    let accounts = AM.fromList $ tokens <&> \token -> ((V1.Role "x", token), 1)
                    (V1.Close, Just (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts})

              expected =
                if null tokens
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
                      , let isAda = symbol' == "" && name' == ""
                      , let badToken = symbol' == "" && name' /= ""
                      , let badCurrency = Plutus.lengthOfByteString symbol' /= 28
                      , let badName = Plutus.lengthOfByteString name' > 32
                      , not isAda
                      , badToken || badCurrency || badName
                      ]
              roleTokensConfig = RoleTokensUsePolicy mockPolicyId mempty
              datum =
                V1.MarloweData
                  { marloweParams = V1.MarloweParams{rolesCurrency = policyIdFromRolesConfig roleTokensConfig}
                  , marloweContract = contract
                  , marloweState = fromMaybe (V1.emptyState 0) state
                  }
              actual = checkContract testnet (Just roleTokensConfig) version datum continuations
          counterexample ("Contract = " <> show contract)
            . counterexample ("State = " <> show state)
            . counterexample ("Actual = " <> show actual)
            . counterexample ("Expected = " <> show expected)
            $ actual `same` expected
        prop "Contract with missing role currency but role token" $ \doTestContract -> do
          let role = V1.Role "x"
              (contract, state) =
                if doTestContract
                  then (V1.Pay role (V1.Account role) ada (V1.Constant 1) V1.Close, Nothing)
                  else do
                    let accounts = AM.fromList [((role, ada), 1)]
                    (V1.Close, Just (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts})
              roleTokensConfig = RoleTokensNone
              state' = fromMaybe (V1.emptyState 0) state

              datum =
                V1.MarloweData
                  { marloweParams = V1.MarloweParams{rolesCurrency = policyIdFromRolesConfig roleTokensConfig}
                  , marloweContract = contract
                  , marloweState = state'
                  }
              expected = [MissingRolesCurrency]
              actual = checkContract testnet (Just roleTokensConfig) version datum continuations
              remapContinuations
                :: M.Map Chain.DatumHash contract
                -> M.Map Plutus.DatumHash contract
              remapContinuations = M.mapKeys $ Plutus.DatumHash . Plutus.toBuiltin . Chain.unDatumHash
              continuations' = remapContinuations continuations
          counterexample ("Contract = " <> show contract)
            . counterexample ("ExtractedRoleNames = " <> (show $ V1.extractRoleNames state' contract continuations'))
            . counterexample ("ExtractedParties = " <> (show $ V1.extractParties state' contract continuations'))
            . counterexample ("State = " <> show state)
            . counterexample ("Actual = " <> show actual)
            . counterexample ("Expected = " <> show expected)
            $ actual `same` expected
        prop "Contract with missing continuation" $ \nonMerkleized ->
          do
            let V1.MerkleizedContract{..} = V1.merkleizedContract $ V1.deepMerkleize nonMerkleized
                toChainDatumHash (Plutus.DatumHash x) = Chain.DatumHash $ Plutus.fromBuiltin x
                continuations' = M.toList $ M.mapKeys toChainDatumHash mcContinuations
            missing <- if null continuations' then pure mempty else pure <$> elements continuations'
            let remaining = filter (`notElem` missing) continuations'
                relevant ContractHasNoRoles = False
                relevant (RoleNameTooLong _) = False
                relevant (InvalidCurrencySymbol _) = False
                relevant (TokenNameTooLong _) = False
                relevant (InvalidToken _) = False
                relevant _ = True
                roleTokensConfig = RoleTokensUsePolicy mockPolicyId mempty
                datum = datumForContractAndRolesConfig mcContract roleTokensConfig
                wholeReport =
                  filter relevant $
                    checkContract testnet (Just roleTokensConfig) version datum (M.fromList remaining)
                actual = filter relevant wholeReport
                remapContinuations
                  :: M.Map Chain.DatumHash contract
                  -> M.Map Plutus.DatumHash contract
                remapContinuations = M.mapKeys $ Plutus.DatumHash . Plutus.toBuiltin . Chain.unDatumHash
                checkResult = checkContinuations mcContract (remapContinuations $ M.fromList remaining)
                expected = MissingContinuation . Plutus.DatumHash . Plutus.toBuiltin . Chain.unDatumHash . fst <$> missing
                -- Illegal addresses are generated by Arbitrary instance but we want to ignore them in this test.
                actual' = flip filter actual \case
                  IllegalAddress _ -> False
                  _ -> True
            pure
              . counterexample ("Contract = " <> show mcContract)
              . counterexample ("DirectCheckResult = " <> show checkResult)
              . counterexample ("Missing = " <> show missing)
              . counterexample ("Remaining = " <> show remaining)
              . counterexample ("WholeReport = " <> show wholeReport)
              . counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual' `same` expected
        prop "Contract with inconsistent networks" $ \address ->
          do
            let contract =
                  V1.When
                    [V1.Case (V1.Deposit (V1.Address True address) (V1.Address False address) (V1.Token "" "") (V1.Constant 1)) V1.Close]
                    0
                    V1.Close
                datum = datumForContractAndRolesConfig contract RoleTokensNone
                actual = checkContract testnet (Just RoleTokensNone) version datum mempty
                expected = case V1.deserialiseAddressBech32 $ V1.serialiseAddressBech32 False address of
                  Just _ -> [WrongNetwork, InconsistentNetworks]
                  Nothing -> [WrongNetwork, InconsistentNetworks, IllegalAddress address]
            counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected
        prop "Contract on wrong network" $ \address ->
          do
            let contract = V1.When [V1.Case (V1.Choice (V1.ChoiceId "Choice" $ V1.Address V1.mainnet address) []) V1.Close] 0 V1.Close
                datum = datumForContractAndRolesConfig contract RoleTokensNone
                actual = checkContract testnet (Just RoleTokensNone) version datum mempty
                expected = case V1.deserialiseAddressBech32 $ V1.serialiseAddressBech32 False address of
                  Just _ -> [WrongNetwork]
                  Nothing -> [WrongNetwork, IllegalAddress address]
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
                datum = datumForContractAndRolesConfig contract RoleTokensNone
                actual = checkContract testnet (Just RoleTokensNone) version datum mempty
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
              fromMaybe (Chain.Lovelace 1_500_000) $
                minAdaUpperBound
                  babbageEraOnwardsTest
                  protocolTestnet
                  version
                  (V1.emptyState 0)
                  contract
                  mempty
        datum <-
          either (error "Unable to initialize the state") pure $
            initialMarloweDatum
              contract
              (RolesPolicyId policy)
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
          -- Overspending or warnings are not a test failures.
          Right errs
            | all overspentOrWarning errs -> pure ()
          -- An ambiguous time interval occurs when the timeouts have non-zero milliseconds are too close for there to be a valid slot for a transaction.
          Left "ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed TEAmbiguousTimeIntervalError)" -> pure ()
          -- All other results are test failures.
          _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
      prop
        "Contract with prohibitive execution cost"
        do
          let minAda = Chain.Lovelace 3_000_000
              policy = "46e79d4fbf0dd6766f8601fdec651ad708af7115fd8f7b5e14b622e5"
              address = "608db2b806ba9e7ae2909ae38afc6c1bce02f5df3e1cb1b06cbc80546f"
              rolesPolicyId = RolesPolicyId policy
              contract =
                V1.When
                  [ V1.Case (V1.Deposit party party ada $ V1.Constant i) $ V1.Pay party (V1.Party party) ada (V1.Constant i) V1.Close
                  | i <- [1 .. 60]
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
              | not (null errs) && all overspent errs -> pure ()
            _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
      prop
        "Contract with prohibitive execution closure cost"
        do
          let initialAccounts :: V1.Accounts
              initialAccounts =
                AM.fromList $
                  [ ((accountId, ada), 1)
                  | i <- [1 .. 10 :: Int]
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
              babbageEraOnwardsTest
              version
              marloweContext
              emptyLockedRolesContext
              address
              datum
              continuations
          case actual of
            Right [] -> expectationFailure "Budget overspending expected"
            Right [TransactionValidationError t1 _, TransactionValidationError t2 _] -> do
              let S.Transaction _ _ i1 _ _ = t1
                  S.Transaction _ _ i2 _ _ = t2
                  -- We can reach `Close` either by the `Notify` or by the `Close` timeout.
                  expectedInputs = [[V1.NormalInput V1.INotify], []]

              map V1.txInputs [i1, i2] `shouldBe` expectedInputs

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
              minAda = Chain.Lovelace 3_000_000

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
            Left "ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed TEAmbiguousTimeIntervalError)" -> pure ()
            -- All other results are test failures.
            _otherwise -> expectationFailure $ "Unexpected result: " <> show actual
