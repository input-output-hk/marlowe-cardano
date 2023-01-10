{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec
  ( spec
  ) where

import Cardano.Api (AddressAny(AddressShelley), TxMetadata(..), TxMetadataValue)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Gen.Cardano.Api.Metadata (genTxMetadata)
import Gen.Cardano.Api.Typed (genAddressShelley)
import qualified Language.Marlowe.Core.V1.Semantics as Semantics
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressAny)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, TransactionMetadata(unTransactionMetadata), toUTxOsList)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (Contract, Datum, MarloweVersion(..), MarloweVersionTag(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusValue)
import Language.Marlowe.Runtime.Transaction.Api (CreateError, RoleTokensConfig(..), mkMint, unMint)
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import Language.Marlowe.Runtime.Transaction.BuildConstraints (buildCreateConstraints)
import qualified Language.Marlowe.Runtime.Transaction.BuildConstraints as BuildConstraints
import Language.Marlowe.Runtime.Transaction.Constraints
  ( MarloweInputConstraints(..)
  , MarloweOutputConstraints(..)
  , RoleTokenConstraints(..)
  , TxConstraints(..)
  , WalletContext(..)
  )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Language.Marlowe.Runtime.Transaction.ConstraintsSpec (genRole, genTransactionOutput, genTxOutRef, shrinkAssets)
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, shouldBe)
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.QuickCheck as Hspec.QuickCheck
import Test.QuickCheck (Arbitrary(..), Property, discard, genericShrink, listOf1, oneof, suchThat, (===))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

byteStringGen :: QuickCheck.Gen ByteString
byteStringGen = BS.pack <$> QuickCheck.arbitrary

spec :: Spec
spec = do
  createSpec
  withdrawSpec

createSpec :: Spec
createSpec = Hspec.describe "buildCreateConstraints" do
  emptyStateProp "writes state with empty choices to marlowe output" $ const Semantics.choices
  emptyStateProp "writes state with empty bound values to marlowe output" $ const Semantics.boundValues
  emptyStateProp "writes state with min time 0 to marlowe output" $ const Semantics.minTime
  Hspec.QuickCheck.prop "writes the contract to the marlowe output" \(SomeCreateArgs args) ->
    let result = extractMarloweDatum <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> (fmap Semantics.marloweContract <$> result) === (Right $ Just $ contract args)
      :: Property
  Hspec.QuickCheck.prop "sends the minAda deposit to the marlowe output" \(SomeCreateArgs args) ->
    let result = extractMarloweAssets <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === (Right $ Just $ Chain.Assets (minAda args) mempty)
      :: Property
  Hspec.QuickCheck.prop "sends minted role tokens" \(SomeCreateArgs args) ->
    case roleTokensConfig args of
      RoleTokensMint mint ->
        let
          result = extractSentRoleTokens <$> runBuildCreateConstraints args
          expected = fst <$> unMint mint
        in case version args of
          MarloweV1 -> result === Right expected
          :: Property
      _ -> discard
  Hspec.QuickCheck.prop "total balance == marlowe output assets" \(SomeCreateArgs args) ->
    let
      result = runBuildCreateConstraints args
      mDatum = extractMarloweDatum <$> result
      mAssets = extractMarloweAssets <$> result
    in case version args of
      MarloweV1 -> (fmap (fromPlutusValue . Semantics.totalBalance . Semantics.accounts . Semantics.marloweState) <$> mDatum) === mAssets
      :: Property
  Hspec.QuickCheck.prop "Doesn't send any payments to addresses" \(SomeCreateArgs args) ->
    let result = payToAddresses <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === Right mempty
      :: Property
  Hspec.QuickCheck.prop "Doesn't send any payments to roles" \(SomeCreateArgs args) ->
    let result = payToRoles <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === Right mempty
      :: Property
  Hspec.QuickCheck.prop "Doesn't consume any roles" \(SomeCreateArgs args) ->
    let result = payoutInputConstraints <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === Right mempty
      :: Property
  Hspec.QuickCheck.prop "Doesn't consume a marlowe input" \(SomeCreateArgs args) ->
    let result = marloweInputConstraints <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === Right MarloweInputConstraintsNone
      :: Property
  Hspec.QuickCheck.prop "Doesn't require extra signatures" \(SomeCreateArgs args) ->
    let result = signatureConstraints <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === Right mempty
      :: Property
  Hspec.QuickCheck.prop "Writes the correct metadata" \(SomeCreateArgs args) ->
    let result = metadataConstraints <$> runBuildCreateConstraints args
    in case version args of
      MarloweV1 -> result === Right (unTransactionMetadata $ metadata args)
      :: Property

  where
    emptyStateProp :: (Eq a, Show a) => String -> (CreateArgs 'V1 -> Semantics.State -> a) -> Spec
    emptyStateProp name f = Hspec.QuickCheck.prop name \(SomeCreateArgs args) ->
      let result = extractMarloweDatum <$> runBuildCreateConstraints args
      in case version args of
          MarloweV1 -> on (===) ((fmap . fmap) (f args))
            (fmap Semantics.marloweState <$> result)
            (Right (Just $ Semantics.emptyState 0))
        :: Property

extractMarloweDatum :: TxConstraints v -> Maybe (Datum v)
extractMarloweDatum TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput _ datum -> Just datum
  _ -> Nothing

extractSentRoleTokens :: TxConstraints v -> Map Chain.TokenName Chain.Address
extractSentRoleTokens TxConstraints{..} = case roleTokenConstraints of
  MintRoleTokens _ _ distribution -> Map.mapKeys Chain.tokenName distribution
  _ -> mempty

extractMarloweAssets :: TxConstraints v -> Maybe Chain.Assets
extractMarloweAssets TxConstraints{..} = case marloweOutputConstraints of
  MarloweOutput assets _ -> Just assets
  _ -> Nothing

runBuildCreateConstraints :: CreateArgs v -> Either (CreateError v) (TxConstraints v)
runBuildCreateConstraints CreateArgs{..} =
  snd <$> buildCreateConstraints version walletContext roleTokensConfig metadata minAda contract

data CreateArgs v = CreateArgs
  { version :: MarloweVersion v
  , walletContext :: WalletContext
  , roleTokensConfig :: RoleTokensConfig
  , metadata :: TransactionMetadata
  , minAda :: Lovelace
  , contract :: Contract v
  } deriving (Generic)

instance Arbitrary (CreateArgs 'V1) where
  arbitrary = CreateArgs MarloweV1
    <$> arbitrary `suchThat` notEmptyWalletContext
    <*> arbitrary
    <*> arbitrary
    <*> (Chain.Lovelace <$> arbitrary)
    <*> arbitrary
  shrink args@CreateArgs{..} = concat
    [ [ args { walletContext = x } | x <- shrink walletContext ]
    , [ args { roleTokensConfig = x } | x <- shrink roleTokensConfig ]
    , [ args { metadata = x } | x <- shrink metadata ]
    , [ args { contract = x } | x <- shrink contract ]
    ]

instance Arbitrary WalletContext where
  arbitrary = WalletContext <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Chain.UTxOs where
  arbitrary = Chain.UTxOs <$> arbitrary
  shrink = genericShrink

instance Arbitrary Chain.TxOutRef where
  arbitrary = genTxOutRef

instance Arbitrary Chain.TransactionOutput where
  arbitrary = genTransactionOutput arbitrary
  shrink txOut = [ txOut { Chain.assets = assets' } | assets' <- shrinkAssets $ Chain.assets txOut ]

instance Arbitrary Chain.Address where
  arbitrary = hedgehog $ fromCardanoAddressAny . AddressShelley <$> genAddressShelley

instance Arbitrary RoleTokensConfig where
  arbitrary = oneof
    [ pure RoleTokensNone
    , RoleTokensUsePolicy . Chain.PolicyId <$> byteStringGen
    , RoleTokensMint . mkMint . NE.fromList <$> listOf1 ((,) <$> genRole <*> ((,Left 1) <$> arbitrary))
    ]
  shrink = \case
    RoleTokensNone -> []
    RoleTokensUsePolicy _ -> [RoleTokensNone]
    RoleTokensMint _ -> [RoleTokensNone]

instance Arbitrary Chain.TransactionMetadata where
  arbitrary = hedgehog $ Chain.TransactionMetadata . fmap Chain.fromCardanoMetadata . unTxMetadata <$> genTxMetadata

unTxMetadata :: TxMetadata -> Map Word64 TxMetadataValue
unTxMetadata (TxMetadata m) = m

notEmptyWalletContext :: WalletContext -> Bool
notEmptyWalletContext WalletContext{..} = not $ null $ toUTxOsList availableUtxos

deriving instance Show (CreateArgs 'V1)

data SomeCreateArgs = forall v. SomeCreateArgs (CreateArgs v)

instance Arbitrary SomeCreateArgs where
  arbitrary = SomeCreateArgs @'V1 <$> arbitrary
  shrink (SomeCreateArgs args) = case version args of
    MarloweV1 -> SomeCreateArgs <$> shrink args

instance Show SomeCreateArgs where
  showsPrec p (SomeCreateArgs a) = case version a of
    MarloweV1 -> showsPrec p a

withdrawSpec :: Spec
withdrawSpec = Hspec.describe "buildWithdrawConstraints" do
  Hspec.QuickCheck.prop "implements Marlowe V1" do
    tokenName <- Chain.TokenName <$> byteStringGen
    policyId <- Chain.PolicyId <$> byteStringGen

    let assetId :: Chain.AssetId
        assetId = Chain.AssetId policyId tokenName

        actual :: Either (Transaction.Api.WithdrawError 'Core.Api.V1) (TxConstraints 'Core.Api.V1)
        actual = BuildConstraints.buildWithdrawConstraints Core.Api.MarloweV1 assetId

        expected :: Either (Transaction.Api.WithdrawError 'Core.Api.V1) (TxConstraints 'Core.Api.V1)
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
