{-# LANGUAGE GADTs #-}

module Language.Marlowe.Object.LinkSpec (spec) where

import Control.Arrow (Arrow (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict as HashMap
import Language.Marlowe.Object.Gen ()
import Language.Marlowe.Object.Link
import Language.Marlowe.Object.Types
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, discard, elements, forAllShrink, (===))

spec :: Spec
spec = do
  prop "linkObject . fromCore == id" \lbl linkedObj ->
    runIdentity (linkObject (fromLinkedObject lbl linkedObj) (pure . (id &&& id)) mempty)
      === Right (linkedObj, HashMap.singleton lbl linkedObj)

  prop "self reference" \lbl label' linkedObject ->
    if lbl == label'
      then discard
      else
        let bundle = ObjectBundle [fromLinkedObject label' linkedObject, mkRefFromLinkedObject lbl label' linkedObject]
            results = [(label', linkedObject), (lbl, linkedObject)]
         in runIdentity (linkBundle' bundle (pure . (id &&& id)) mempty)
              === Right (results, HashMap.fromList results)

  prop "UnknownSymbol" \lbl someObjectType ->
    runIdentity (linkObject (mkRefFromType lbl someObjectType) (pure . (id &&& id)) $ HashMap.delete lbl mempty)
      === Left (UnknownSymbol lbl)

  prop "DuplicateSymbol" \labelledObject linkedObject ->
    runIdentity (linkObject labelledObject (pure . (id &&& id)) $ HashMap.singleton (_label labelledObject) linkedObject)
      === Left (DuplicateLabel $ _label labelledObject)

  prop "TypeMismatch" \lbl label' linkedObject ->
    if lbl == label'
      then discard
      else forAllShrink (genTypeMismatch lbl label' linkedObject) shrink \labelledObject ->
        let bundle = ObjectBundle [fromLinkedObject label' linkedObject, labelledObject]
         in linkBundle bundle
              === Left (TypeMismatch (labelledObjectType labelledObject) (linkedObjectType linkedObject))

  prop "linkObject . unlink == id" \linkedObject ->
    let (lbl, bundle) = unlink linkedObject
     in (lookup lbl <$> linkBundle bundle) === Right (Just linkedObject)

genTypeMismatch :: Label -> Label -> LinkedObject -> Gen LabelledObject
genTypeMismatch lbl label' =
  elements . \case
    LinkedAction _ ->
      [ LabelledObject lbl ContractType $ ContractRef label'
      , LabelledObject lbl ObservationType $ ObservationRef label'
      , LabelledObject lbl PartyType $ PartyRef label'
      , LabelledObject lbl TokenType $ TokenRef label'
      , LabelledObject lbl ValueType $ ValueRef label'
      ]
    LinkedContract _ ->
      [ LabelledObject lbl ActionType $ ActionRef label'
      , LabelledObject lbl ObservationType $ ObservationRef label'
      , LabelledObject lbl PartyType $ PartyRef label'
      , LabelledObject lbl TokenType $ TokenRef label'
      , LabelledObject lbl ValueType $ ValueRef label'
      ]
    LinkedObservation _ ->
      [ LabelledObject lbl ContractType $ ContractRef label'
      , LabelledObject lbl ActionType $ ActionRef label'
      , LabelledObject lbl PartyType $ PartyRef label'
      , LabelledObject lbl TokenType $ TokenRef label'
      , LabelledObject lbl ValueType $ ValueRef label'
      ]
    LinkedParty _ ->
      [ LabelledObject lbl ContractType $ ContractRef label'
      , LabelledObject lbl ObservationType $ ObservationRef label'
      , LabelledObject lbl ActionType $ ActionRef label'
      , LabelledObject lbl TokenType $ TokenRef label'
      , LabelledObject lbl ValueType $ ValueRef label'
      ]
    LinkedToken _ ->
      [ LabelledObject lbl ContractType $ ContractRef label'
      , LabelledObject lbl ObservationType $ ObservationRef label'
      , LabelledObject lbl PartyType $ PartyRef label'
      , LabelledObject lbl ActionType $ ActionRef label'
      , LabelledObject lbl ValueType $ ValueRef label'
      ]
    LinkedValue _ ->
      [ LabelledObject lbl ContractType $ ContractRef label'
      , LabelledObject lbl ObservationType $ ObservationRef label'
      , LabelledObject lbl PartyType $ PartyRef label'
      , LabelledObject lbl TokenType $ TokenRef label'
      , LabelledObject lbl ActionType $ ActionRef label'
      ]

mkRefFromLinkedObject :: Label -> Label -> LinkedObject -> LabelledObject
mkRefFromLinkedObject lbl label' = \case
  LinkedAction _ -> LabelledObject lbl ActionType $ ActionRef label'
  LinkedContract _ -> LabelledObject lbl ContractType $ ContractRef label'
  LinkedObservation _ -> LabelledObject lbl ObservationType $ ObservationRef label'
  LinkedParty _ -> LabelledObject lbl PartyType $ PartyRef label'
  LinkedToken _ -> LabelledObject lbl TokenType $ TokenRef label'
  LinkedValue _ -> LabelledObject lbl ValueType $ ValueRef label'

mkRefFromType :: Label -> SomeObjectType -> LabelledObject
mkRefFromType lbl (SomeObjectType type_) = case type_ of
  ActionType -> LabelledObject lbl type_ $ ActionRef lbl
  ContractType -> LabelledObject lbl type_ $ ContractRef lbl
  ObservationType -> LabelledObject lbl type_ $ ObservationRef lbl
  PartyType -> LabelledObject lbl type_ $ PartyRef lbl
  TokenType -> LabelledObject lbl type_ $ TokenRef lbl
  ValueType -> LabelledObject lbl type_ $ ValueRef lbl
