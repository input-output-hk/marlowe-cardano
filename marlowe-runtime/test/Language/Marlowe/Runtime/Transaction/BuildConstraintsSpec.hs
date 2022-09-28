{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import qualified Language.Marlowe.Runtime.Transaction.BuildConstraints as BuildConstraints
import Language.Marlowe.Runtime.Transaction.Constraints (TxConstraints(..))
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Test.Hspec (Spec, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.QuickCheck as Hspec.QuickCheck
import qualified Test.QuickCheck as QuickCheck

byteStringGen :: QuickCheck.Gen ByteString
byteStringGen = BS.pack <$> QuickCheck.arbitrary

spec :: Spec
spec = Hspec.describe "buildWithdrawConstraints" do
  Hspec.QuickCheck.prop "implements Marlowe V1" do
    tokenName <- Chain.TokenName <$> byteStringGen
    policyId <- Chain.PolicyId <$> byteStringGen

    let assetId :: Chain.AssetId
        assetId = Chain.AssetId policyId tokenName

        actual :: Either (Transaction.Api.WithdrawError v) (TxConstraints 'Core.Api.V1)
        actual = BuildConstraints.buildWithdrawConstraints Core.Api.MarloweV1 assetId

        expected :: Either (Transaction.Api.WithdrawError v) (TxConstraints 'Core.Api.V1)
        expected = Right $ TxConstraints
          { marloweInputConstraints = TxConstraints.MarloweInputConstraintsNone
          , payoutInputConstraints = Set.singleton assetId
          , roleTokenConstraints = TxConstraints.SpendRoleTokens $ Set.singleton assetId
          , payToAddresses = Map.empty
          , payToRoles = Map.empty
          , marloweOutputConstraints = TxConstraints.MarloweOutputConstraintsNone
          , signatureConstraints = Set.empty
          , metadataConstraints = Map.empty
          }

    pure $ actual `shouldBe` expected
