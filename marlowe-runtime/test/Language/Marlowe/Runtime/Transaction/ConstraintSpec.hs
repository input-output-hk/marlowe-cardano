{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Marlowe.Runtime.Transaction.ConstraintSpec
  where

import Cardano.Api (TxBody(..), TxBodyContent(..))
import qualified Cardano.Api as C
import Data.List (find)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Cardano.Api
  ( fromCardanoAddressInEra
  , fromCardanoAssetName
  , fromCardanoPolicyId
  , toCardanoAssetName
  , toCardanoPolicyId
  , toCardanoTxIn
  )
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Transaction.Constraints
  (MarloweContext, TxConstraints, UnsolvableConstraintsError, WalletContext, mustMintRoleToken)
import Test.QuickCheck
  (Arbitrary, Gen, Property, Testable(property), arbitrary, discard, frequency, resize, sized, (===))

-- A set of TxConstraints with the necessary context to solve them.
data TxConstraintsInContext v =
  TxConstraintsInContext (MarloweContext v) WalletContext (TxConstraints v)

deriving instance Eq (TxConstraintsInContext 'V1)
deriving instance Show (TxConstraintsInContext 'V1)

instance IsMarloweVersion v => Semigroup (TxConstraintsInContext v) where
  TxConstraintsInContext mCtx1 wCtx1 c1 <> TxConstraintsInContext mCtx2 wCtx2 c2 =
    TxConstraintsInContext
      (mCtx1 <> mCtx2)
      (wCtx1 <> wCtx2)
      (c1 <> c2)

instance IsMarloweVersion v => Arbitrary (TxConstraintsInContext v) where
  arbitrary = sized \size -> frequency
    [ (size, resize (size `div` 2) $ (<>) <$> arbitrary <*> arbitrary)
    , (1, genMustMintRoleToken)
    ]

genMustMintRoleToken :: Gen (TxConstraintsInContext v)
genMustMintRoleToken = error "not implemented"

-- mustMintRoleToken Properties

propMustMintRoleTokenMintsOneToken
  :: IsMarloweVersion v
  => (TxConstraints v -> Either UnsolvableConstraintsError (TxBody era))
  -> TxConstraints v
  -> TxOutRef
  -> AssetId
  -> Address
  -> Property
propMustMintRoleTokenMintsOneToken solver constraints ref assetId address =
  solver constraints' `rightImplies` \txBody -> quantityMinted txBody === 1
  where
    constraints' = constraints <> mustMintRoleToken ref assetId address
    quantityMinted (TxBody TxBodyContent{..}) = case txMintValue of
      C.TxMintNone -> 0
      C.TxMintValue _ value _ -> case toCardanoPolicyId $ policyId assetId of
        Nothing -> 0
        Just policyId' -> C.selectAsset value
          $ C.AssetId policyId'
          $ toCardanoAssetName
          $ tokenName assetId

propMustMintRoleTokenCorrectAddress
  :: C.IsCardanoEra era
  => IsMarloweVersion v
  => (TxConstraints v -> Either UnsolvableConstraintsError (TxBody era))
  -> TxConstraints v
  -> TxOutRef
  -> AssetId
  -> Address
  -> Property
propMustMintRoleTokenCorrectAddress solver constraints ref assetId address =
  solver constraints' `rightImplies` \txBody -> distribution txBody === Set.singleton address
  where
    constraints' = constraints <> mustMintRoleToken ref assetId address
    distribution (TxBody TxBodyContent{..}) = flip foldMap txOuts \case
      C.TxOut addressInEra txOutValue _ _ -> case txOutValue of
        C.TxOutAdaOnly _ _ -> mempty
        C.TxOutValue _ value -> case toCardanoPolicyId $ policyId assetId of
          Nothing -> mempty
          Just policyId'
            | tokensSent value policyId' > 0 -> Set.singleton $ fromCardanoAddressInEra addressInEra
            | otherwise -> mempty
    tokensSent value policyId = C.selectAsset value
      $ C.AssetId policyId
      $ toCardanoAssetName
      $ tokenName assetId

propMustMintRoleTokenSendsOneToken
  :: IsMarloweVersion v
  => (TxConstraints v -> Either UnsolvableConstraintsError (TxBody era))
  -> TxConstraints v
  -> TxOutRef
  -> AssetId
  -> Address
  -> Property
propMustMintRoleTokenSendsOneToken solver constraints ref assetId address =
  solver constraints' `rightImplies` \txBody -> quantitySent txBody === 1
  where
    constraints' = constraints <> mustMintRoleToken ref assetId address
    quantitySent (TxBody TxBodyContent{..}) = case toCardanoPolicyId $ policyId assetId of
      Nothing -> 0
      Just policyId' -> flip foldMap txOuts \case
        C.TxOut _ txOutValue _ _ -> case txOutValue of
          C.TxOutAdaOnly _ _ -> 0
          C.TxOutValue _ value -> tokensSent value policyId'
    tokensSent value policyId = C.selectAsset value
      $ C.AssetId policyId
      $ toCardanoAssetName
      $ tokenName assetId

propMustMintRoleTokenNoExtraTokens
  :: IsMarloweVersion v
  => (TxConstraints v -> Either UnsolvableConstraintsError (TxBody era))
  -> TxConstraints v
  -> TxOutRef
  -> AssetId
  -> Address
  -> Property
propMustMintRoleTokenNoExtraTokens solver constraints ref assetId address =
  solver constraints' `rightImplies` \txBody ->
    outputWithToken txBody `justImplies` \txOut -> tokensSent txOut === Set.singleton assetId
  where
    constraints' = constraints <> mustMintRoleToken ref assetId address
    outputWithToken (TxBody TxBodyContent{..}) = find sendsToken txOuts
    sendsToken (C.TxOut _ txOutValue _ _) = case txOutValue of
      C.TxOutAdaOnly _ _ -> False
      C.TxOutValue _ value -> any (tokenMatches . fst) $ C.valueToList value
    tokenMatches = \case
      C.AdaAssetId -> False
      C.AssetId policy name -> policyId assetId == fromCardanoPolicyId policy
        && tokenName assetId == fromCardanoAssetName name
    tokensSent (C.TxOut _ txOutValue _ _) = case txOutValue of
      C.TxOutAdaOnly _ _ -> mempty
      C.TxOutValue _ value -> foldMap (extractAssetId . fst) $ C.valueToList value
    extractAssetId = \case
      C.AdaAssetId -> mempty
      C.AssetId policy name -> Set.singleton AssetId
        { policyId = fromCardanoPolicyId policy
        , tokenName = fromCardanoAssetName name
        }

propMustMintRoleTokenConsumesTxOutRef
  :: IsMarloweVersion v
  => (TxConstraints v -> Either UnsolvableConstraintsError (TxBody era))
  -> TxConstraints v
  -> TxOutRef
  -> AssetId
  -> Address
  -> Property
propMustMintRoleTokenConsumesTxOutRef solver constraints ref assetId address =
  solver constraints' `rightImplies` \txBody ->
    toCardanoTxIn ref `justImplies` \txIn -> property $ Set.member txIn $ txInSet txBody
  where
    constraints' = constraints <> mustMintRoleToken ref assetId address
    txInSet (TxBody TxBodyContent{..}) = Set.fromList $ fmap fst txIns

-- helpers

rightImplies :: Either e a -> (a -> Property) -> Property
rightImplies = \case
  Left _ -> const discard
  Right a -> ($ a)

justImplies :: Maybe a -> (a -> Property) -> Property
justImplies = \case
  Nothing -> const discard
  Just a -> ($ a)
