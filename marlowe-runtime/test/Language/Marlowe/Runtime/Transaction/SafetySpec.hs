
{-# LANGUAGE OverloadedStrings #-}


module Language.Marlowe.Runtime.Transaction.SafetySpec
  where


import Data.List (nub)
import Data.Maybe (fromJust)
import Language.Marlowe.Analysis.Safety.Types
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1))
import Language.Marlowe.Runtime.Transaction.Api (Mint(..), RoleTokensConfig(..))
import Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec ()
import Language.Marlowe.Runtime.Transaction.ConstraintsSpec (protocolTestnet)
import Language.Marlowe.Runtime.Transaction.Safety
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample, discard, sublistOf, suchThat, (===))

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Shelley
import qualified Data.Map.Strict as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.Cardano.Api as Chain
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified PlutusTx.Builtins as Plutus


spec :: Spec
spec =
  do

    let
      version = MarloweV1
      noContinuations' = noContinuations version
      party = V1.Role "x"
      payee = V1.Party party
      payToken token = V1.Pay party payee token $ V1.Constant 1
      payRole role = V1.Pay (V1.Role role) (V1.Party $ V1.Role role) (V1.Token "" "") $ V1.Constant 1
      same x y = nub x == x && nub y == y && null (filter (`notElem` x) y) && null (filter (`notElem` y) x)

    describe "minAdaUpperBound"
      $ do
          let
          prop "At least Cardano.Api" $ \(address, hash, assets@(Chain.Assets _ tokens')) ->
            do
              let
                value = fromJust $ Chain.assetsToCardanoValue assets
                toToken (Chain.AssetId (Chain.PolicyId p) (Chain.TokenName n)) =
                  V1.Token
                    (Plutus.CurrencySymbol $ Plutus.toBuiltin p)
                    (Plutus.TokenName $ Plutus.toBuiltin n)
                tokens = fmap toToken . M.keys $ Chain.unTokens tokens'
                expected =
                  either (const 0) Cardano.selectLovelace
                    $ Cardano.calculateMinimumUTxO
                        Cardano.ShelleyBasedEraBabbage
                        (
                          Cardano.TxOut
                            (Cardano.anyAddressInShelleyBasedEra . fromJust $ Chain.toCardanoAddress address)
                            (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra value)
                            (Cardano.TxOutDatumHash Cardano.ScriptDataInBabbageEra . fromJust $ Chain.toCardanoDatumHash hash)
                            Shelley.ReferenceScriptNone
                        )
                        protocolTestnet
                   :: Cardano.Lovelace
                contract = foldr payToken V1.Close tokens  -- The tokens just need to appear somewhere in the contract.
                actual = fromJust $ minAdaUpperBound protocolTestnet version contract noContinuations' :: Cardano.Lovelace
              counterexample ("Expected minUTxO = " <> show expected)
                $ counterexample ("Actual minUTxO = " <> show actual)
                $ actual >= expected

    describe "checkContract"
      $ do
        prop "Contract without roles" $ \roleTokensConfig ->
          let
            contract = V1.Close
            actual = checkContract roleTokensConfig version contract noContinuations'
          in
            counterexample ("Contract = " <> show contract)
              $ case roleTokensConfig of
                  RoleTokensNone -> actual === []
                  _              -> actual === [ContractHasNoRoles]
        prop "Contract with roles from minting" $ \roleTokensConfig ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              let
                roles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (unMint mint)
                contract = foldr payRole V1.Close roles
                actual = checkContract roleTokensConfig version contract noContinuations'
              in
                counterexample ("Contract = " <> show contract)
                  $ actual === mempty
            _ -> discard
        prop "Contract with roles missing from minting" $ \roleTokensConfig extra ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              let
                roles = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (unMint mint)
                contract = foldr payRole V1.Close $ extra <> roles
                actual = checkContract roleTokensConfig version contract noContinuations'
                expected =
                  (MissingRoleToken <$> nub extra)
                    <> [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub extra, Plutus.lengthOfByteString name > 32]
              in
                counterexample ("Contract = " <> show contract)
                  . counterexample ("Actual = " <> show actual)
                  . counterexample ("Expected = " <> show expected)
                  $ actual `same` expected
            _ -> discard
        prop "Contract with extra roles for minting" $ \roleTokensConfig ->
          case roleTokensConfig of
            RoleTokensMint mint ->
              do
                let
                  roles' = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (unMint mint)
                roles <- sublistOf roles' `suchThat` (not . null)
                let
                  extra = filter (`notElem` roles) roles'
                  contract = foldr payRole V1.Close roles
                  actual = checkContract roleTokensConfig version contract noContinuations'
                  expected = ExtraRoleToken <$> extra
                pure
                  . counterexample ("Contract = " <> show contract)
                  . counterexample ("Actual = " <> show actual)
                  . counterexample ("Expected = " <> show expected)
                  $ actual `same` expected
            _ -> discard
        prop "Contract with role name too long" $ \roles ->
          let
            contract = foldr payRole V1.Close roles
            actual = checkContract (RoleTokensUsePolicy "") version contract noContinuations'
            expected =
              if null roles
                then [ContractHasNoRoles]
                else [RoleNameTooLong role | role@(Plutus.TokenName name) <- nub roles, Plutus.lengthOfByteString name > 32]
          in
            counterexample ("Contract = " <> show contract)
              . counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected
        prop "Contract with illegal token" $ \tokens ->
          let
            contract = foldr payToken V1.Close tokens
            actual = checkContract (RoleTokensUsePolicy "") version contract noContinuations'
            expected =
              if contract == V1.Close
                then [ContractHasNoRoles]
                else nub
                     [
                       if badToken
                         then InvalidToken token
                         else if badCurrency
                                then InvalidCurrencySymbol symbol
                                else TokenNameTooLong name
                     |
                       token@(V1.Token symbol@(Plutus.CurrencySymbol symbol') name@(Plutus.TokenName name')) <- nub tokens
                     , let ada = symbol' == "" && name' == ""
                     , let badToken = symbol' == "" && name' /= ""
                     , let badCurrency = Plutus.lengthOfByteString symbol' /= 28
                     , let badName = Plutus.lengthOfByteString name' > 32
                     , not ada
                     , badToken || badCurrency || badName
                     ]
          in
            counterexample ("Contract = " <> show contract)
              . counterexample ("Actual = " <> show actual)
              . counterexample ("Expected = " <> show expected)
              $ actual `same` expected
