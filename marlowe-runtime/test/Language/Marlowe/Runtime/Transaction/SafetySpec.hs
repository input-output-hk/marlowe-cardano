
{-# LANGUAGE OverloadedStrings #-}


module Language.Marlowe.Runtime.Transaction.SafetySpec
  where


import Data.Maybe (fromJust)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1))
import Language.Marlowe.Runtime.Transaction.Api (RoleTokensConfig(..))
import Language.Marlowe.Runtime.Transaction.ConstraintsSpec (protocolTestnet)
import Language.Marlowe.Runtime.Transaction.Safety
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample)

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Shelley
import qualified Data.Map.Strict as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.Cardano.Api as Chain
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Plutus.V2.Ledger.Api as Plutus


spec :: Spec
spec =
  do

    let
      version = MarloweV1
      noContinuations' = noContinuations version
      party = V1.Role "x"
      payee = V1.Party party
      pay token = V1.Pay party payee token $ V1.Constant 1

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
                contract = foldr pay V1.Close tokens  -- The tokens just need to appear somewhere in the contract.
                actual = fromJust $ minAdaUpperBound protocolTestnet version contract noContinuations' :: Cardano.Lovelace
              counterexample ("Expected minUTxO = " <> show expected)
                $ counterexample ("Actual minUTxO = " <> show actual)
                $ actual >= expected
