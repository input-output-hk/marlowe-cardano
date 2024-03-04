{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | Defines functions and type related to linking an object bundle into a collection of core terms.
module Language.Marlowe.Object.Link where

import Cardano.Api (
  Address,
  Hash,
  PaymentCredential (PaymentCredentialByKey, PaymentCredentialByScript),
  PaymentKey,
  ScriptHash,
  SerialiseAsRawBytes (serialiseToRawBytes),
  StakeAddressPointer (..),
  StakeAddressReference (..),
 )
import Cardano.Api.Byron (ShelleyAddr)
import Cardano.Api.Shelley (
  Address (..),
  StakeCredential (..),
  fromShelleyPaymentCredential,
  fromShelleyStakeReference,
 )
import Cardano.Ledger.BaseTypes (Network (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Credential (Ptr (..))
import Control.Lens (Fold, folding, makePrisms, traverseOf_, (^.))
import Control.Monad (when)
import Control.Monad.Trans.RWS (RWS, evalRWS)
import qualified Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.State
import Data.Aeson (FromJSON, ToJSON)
import Data.Base16.Types (extractBase16)
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (Binary)
import Data.ByteString.Base16 (encodeBase16)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as C
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as C
import qualified Language.Marlowe.Object.Types as O
import Language.Marlowe.Util (dataHash)
import Network.Protocol.Codec.Spec (Variations)
import PlutusLedgerApi.V1 (ToData)
import PlutusLedgerApi.V2 (fromBuiltin, toBuiltin)
import qualified PlutusLedgerApi.V2 as PV2

-- | Something that can go wrong when linking.
data LinkError
  = UnknownSymbol O.Label
  | DuplicateLabel O.Label
  | TypeMismatch O.SomeObjectType O.SomeObjectType
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Binary, Variations)

-- | The state of the linker.
type SymbolTable = HashMap O.Label LinkedObject

-- | A linked object, corresponding to the core representation of each object type.
data LinkedObject
  = LinkedValue (C.Value C.Observation)
  | LinkedObservation C.Observation
  | LinkedContract C.Contract
  | LinkedParty C.Party
  | LinkedToken C.Token
  | LinkedAction C.Action
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

-- | A type family that provides a bidirectional mapping of core and object term types.
type family Linked (o :: Type) = (r :: Type) | r -> o where
  Linked O.Value = C.Value C.Observation
  Linked O.Observation = C.Observation
  Linked O.Contract = C.Contract
  Linked O.Party = C.Party
  Linked O.Token = C.Token
  Linked O.Action = C.Action

-- | Link a bundle, producing a list of labels and linked objects.
linkBundle
  :: O.ObjectBundle
  -> Either LinkError [(O.Label, LinkedObject)]
linkBundle bundle = fst <$> runIdentity (linkBundle' bundle (\a -> pure (a, a)) mempty)

-- | A more flexible version of linkBundle, which runs in any Monad and allows terms to be rewritten as they are linked.
-- The rewritten term is stored in the symbol table, and the second value is added to the resulting association list.
linkBundle'
  :: (Monad m)
  => O.ObjectBundle
  -- ^ The object bundle to link.
  -> (LinkedObject -> m (LinkedObject, a))
  -- ^ A function that rewrites a linked object and converts it to a polymorphic result.
  -> SymbolTable
  -- ^ The initial symbol table.
  -> m (Either LinkError ([(O.Label, a)], SymbolTable))
linkBundle' bundle transform = go [] $ O.getObjects bundle
  where
    go acc [] objects = pure $ pure (reverse acc, objects)
    go acc (x : xs) objects = do
      result <- linkObject x transform objects
      case result of
        Left err -> pure $ Left err
        Right (linked, objects') -> go ((x ^. O.label, linked) : acc) xs objects'

-- | A partial inverse of linkBundle. It decomposes a linked object into an object bundle with a naive optimization
-- rule: if the same term appears more than once, replace all instances of it with a reference, otherwise leave it
-- inlined. In some cases, this will make the bundle less space-efficient if the term being replaced is smaller than the
-- size of the label.
unlink :: LinkedObject -> (O.Label, O.ObjectBundle)
unlink linked = (label, O.ObjectBundle $ DList.toList $ DList.snoc bundle labelled)
  where
    labelled = case unlinked of
      O.Object t v -> O.LabelledObject label t v
    label = mkLabel linked
    (unlinked, bundle) = case linked of
      LinkedValue a ->
        first (O.Object O.ValueType) $
          evalRWS (visit O.ValueType a) counts mempty
      LinkedObservation a ->
        first (O.Object O.ObservationType) $
          evalRWS (visit O.ObservationType a) counts mempty
      LinkedContract a ->
        first (O.Object O.ContractType) $
          evalRWS (visit O.ContractType a) counts mempty
      LinkedParty a ->
        first (O.Object O.PartyType) $
          evalRWS (visit O.PartyType a) counts mempty
      LinkedToken a ->
        first (O.Object O.TokenType) $
          evalRWS (visit O.TokenType a) counts mempty
      LinkedAction a ->
        first (O.Object O.ActionType) $
          evalRWS (visit O.ActionType a) counts mempty
    counts = execState (countOccurrences linked) mempty

linkObject
  :: (Monad m)
  => O.LabelledObject
  -> (LinkedObject -> m (LinkedObject, a))
  -> SymbolTable
  -> m (Either LinkError (a, SymbolTable))
linkObject O.LabelledObject{..} transform objects
  | HashMap.member _label objects = pure $ Left $ DuplicateLabel _label
  | otherwise = do
      let mLinked = case _type of
            O.ValueType -> LinkedValue <$> linkValue objects _value
            O.ObservationType -> LinkedObservation <$> linkObservation objects _value
            O.ContractType -> LinkedContract <$> linkContract objects _value
            O.PartyType -> LinkedParty <$> linkParty objects _value
            O.TokenType -> LinkedToken <$> linkToken objects _value
            O.ActionType -> LinkedAction <$> linkAction objects _value
      case mLinked of
        Left err -> pure $ Left err
        Right linked -> do
          (linked', a) <- transform linked
          pure $ Right (a, HashMap.insert _label linked' objects)

linkValue :: SymbolTable -> O.Value -> Either LinkError (C.Value C.Observation)
linkValue objects = \case
  O.AvailableMoney account token ->
    C.AvailableMoney
      <$> linkParty objects account
      <*> linkToken objects token
  O.Constant i -> pure $ C.Constant i
  O.NegValue a -> C.NegValue <$> linkValue objects a
  O.AddValue a b -> C.AddValue <$> linkValue objects a <*> linkValue objects b
  O.SubValue a b -> C.SubValue <$> linkValue objects a <*> linkValue objects b
  O.MulValue a b -> C.MulValue <$> linkValue objects a <*> linkValue objects b
  O.DivValue a b -> C.DivValue <$> linkValue objects a <*> linkValue objects b
  O.ChoiceValue (O.ChoiceId name owner) ->
    C.ChoiceValue . C.ChoiceId (toBuiltin $ T.encodeUtf8 name) <$> linkParty objects owner
  O.TimeIntervalEnd -> pure C.TimeIntervalEnd
  O.TimeIntervalStart -> pure C.TimeIntervalStart
  O.UseValue (O.ValueId val) -> pure $ C.UseValue (C.ValueId $ toBuiltin val)
  O.Cond c a b ->
    C.Cond <$> linkObservation objects c <*> linkValue objects a <*> linkValue objects b
  O.ValueRef lbl -> resolveReference lbl objects \case
    LinkedValue value -> Right value
    _ -> Left $ O.SomeObjectType O.ValueType

linkObservation :: SymbolTable -> O.Observation -> Either LinkError C.Observation
linkObservation objects = \case
  O.AndObs a b -> C.AndObs <$> linkObservation objects a <*> linkObservation objects b
  O.OrObs a b -> C.OrObs <$> linkObservation objects a <*> linkObservation objects b
  O.NotObs a -> C.NotObs <$> linkObservation objects a
  O.ChoseSomething (O.ChoiceId name owner) ->
    C.ChoseSomething . C.ChoiceId (toBuiltin $ T.encodeUtf8 name) <$> linkParty objects owner
  O.ValueGE a b -> C.ValueGE <$> linkValue objects a <*> linkValue objects b
  O.ValueGT a b -> C.ValueGT <$> linkValue objects a <*> linkValue objects b
  O.ValueLE a b -> C.ValueLE <$> linkValue objects a <*> linkValue objects b
  O.ValueLT a b -> C.ValueLT <$> linkValue objects a <*> linkValue objects b
  O.ValueEQ a b -> C.ValueEQ <$> linkValue objects a <*> linkValue objects b
  O.TrueObs -> pure C.TrueObs
  O.FalseObs -> pure C.FalseObs
  O.ObservationRef lbl -> resolveReference lbl objects \case
    LinkedObservation value -> Right value
    _ -> Left $ O.SomeObjectType O.ObservationType

linkContract :: SymbolTable -> O.Contract -> Either LinkError C.Contract
linkContract objects = \case
  O.Close -> pure C.Close
  O.Pay accountId payee token value contract ->
    C.Pay
      <$> linkParty objects accountId
      <*> linkPayee objects payee
      <*> linkToken objects token
      <*> linkValue objects value
      <*> linkContract objects contract
  O.If obs c1 c2 -> C.If <$> linkObservation objects obs <*> linkContract objects c1 <*> linkContract objects c2
  O.When cases timeout fallback ->
    C.When <$> traverse (linkCase objects) cases <*> pure (O.toCoreTimeout timeout) <*> linkContract objects fallback
  O.Let valueId value c ->
    C.Let (O.toCoreValueId valueId) <$> linkValue objects value <*> linkContract objects c
  O.Assert obs c -> C.Assert <$> linkObservation objects obs <*> linkContract objects c
  O.ContractRef lbl -> resolveReference lbl objects \case
    LinkedContract value -> Right value
    _ -> Left $ O.SomeObjectType O.ContractType

linkCase :: SymbolTable -> O.Case -> Either LinkError (C.Case C.Contract)
linkCase objects = \case
  O.Case action c -> C.Case <$> linkAction objects action <*> linkContract objects c
  O.MerkleizedCase action (O.ContractHash hash) ->
    C.MerkleizedCase <$> linkAction objects action <*> pure (toBuiltin hash)

linkPayee :: SymbolTable -> O.Payee -> Either LinkError C.Payee
linkPayee objects = \case
  O.Party p -> C.Party <$> linkParty objects p
  O.Account p -> C.Account <$> linkParty objects p

linkParty :: SymbolTable -> O.Party -> Either LinkError C.Party
linkParty objects = \case
  O.Role token -> pure $ C.Role $ O.toCoreTokenName token
  O.Address (O.ShelleyAddress address) -> pure $ C.Address (getNetwork address) (toPlutusAddress address)
  O.PartyRef lbl -> resolveReference lbl objects \case
    LinkedParty value -> Right value
    _ -> Left $ O.SomeObjectType O.PartyType

getNetwork :: Address ShelleyAddr -> C.Network
getNetwork (ShelleyAddress network _ _) = case network of
  Mainnet -> C.mainnet
  Testnet -> C.testnet

toPlutusAddress :: Address ShelleyAddr -> PV2.Address
toPlutusAddress (ShelleyAddress _ payment staking) =
  PV2.Address
    (toPlutusPayment $ fromShelleyPaymentCredential payment)
    (toPlutusStaking $ fromShelleyStakeReference staking)

toPlutusPayment :: PaymentCredential -> PV2.Credential
toPlutusPayment = \case
  PaymentCredentialByKey keyHash -> PV2.PubKeyCredential $ toPlutusPubKeyHash keyHash
  PaymentCredentialByScript scriptHash -> PV2.ScriptCredential $ toPlutusScriptHash scriptHash

toPlutusScriptHash :: ScriptHash -> PV2.ScriptHash
toPlutusScriptHash = PV2.ScriptHash . PV2.toBuiltin . serialiseToRawBytes

toPlutusPubKeyHash :: Hash PaymentKey -> PV2.PubKeyHash
toPlutusPubKeyHash = PV2.PubKeyHash . PV2.toBuiltin . serialiseToRawBytes

toPlutusStaking :: StakeAddressReference -> Maybe PV2.StakingCredential
toPlutusStaking = \case
  NoStakeAddress -> Nothing
  StakeAddressByValue credential -> Just $ PV2.StakingHash $ case credential of
    StakeCredentialByKey keyHash ->
      PV2.PubKeyCredential $ PV2.PubKeyHash $ PV2.toBuiltin $ serialiseToRawBytes keyHash
    StakeCredentialByScript scriptHash -> PV2.ScriptCredential $ toPlutusScriptHash scriptHash
  StakeAddressByPointer (StakeAddressPointer (Ptr slotNo txIx certIx)) ->
    Just $
      PV2.StakingPtr
        (fromIntegral $ Ledger.unSlotNo slotNo)
        (case txIx of Ledger.TxIx i -> fromIntegral i)
        (case certIx of Ledger.CertIx i -> fromIntegral i)

linkToken :: SymbolTable -> O.Token -> Either LinkError C.Token
linkToken objects = \case
  O.Token currencySymbol tokenName ->
    pure $ C.Token (O.toCoreCurrencySymbol currencySymbol) (O.toCoreTokenName tokenName)
  O.TokenRef lbl -> resolveReference lbl objects \case
    LinkedToken value -> Right value
    _ -> Left $ O.SomeObjectType O.TokenType

linkAction :: SymbolTable -> O.Action -> Either LinkError C.Action
linkAction objects = \case
  O.Deposit account party token value ->
    C.Deposit
      <$> linkParty objects account
      <*> linkParty objects party
      <*> linkToken objects token
      <*> linkValue objects value
  O.Choice (O.ChoiceId name owner) bounds ->
    C.Choice
      <$> (C.ChoiceId (toBuiltin $ T.encodeUtf8 name) <$> linkParty objects owner)
      <*> pure (O.toCoreBound <$> bounds)
  O.Notify obs -> C.Notify <$> linkObservation objects obs
  O.ActionRef lbl -> resolveReference lbl objects \case
    LinkedAction value -> Right value
    _ -> Left $ O.SomeObjectType O.ActionType

resolveReference :: O.Label -> SymbolTable -> (LinkedObject -> Either O.SomeObjectType a) -> Either LinkError a
resolveReference lbl objects handle = case HashMap.lookup lbl objects of
  Nothing -> Left $ UnknownSymbol lbl
  Just object -> case handle object of
    Left expected -> Left $ TypeMismatch expected $ linkedObjectType object
    Right a -> pure a

linkedObjectType :: LinkedObject -> O.SomeObjectType
linkedObjectType = \case
  LinkedValue _ -> O.SomeObjectType O.ValueType
  LinkedObservation _ -> O.SomeObjectType O.ObservationType
  LinkedContract _ -> O.SomeObjectType O.ContractType
  LinkedParty _ -> O.SomeObjectType O.PartyType
  LinkedToken _ -> O.SomeObjectType O.TokenType
  LinkedAction _ -> O.SomeObjectType O.ActionType

visit
  :: O.ObjectType a
  -> Linked a
  -> RWS (HashMap O.Label Int) (DList O.LabelledObject) (HashSet O.Label) a
visit t linked = do
  let lbl = mkLabel $ toLinkedObject t linked
  visited <- RWS.gets $ HashSet.member lbl
  count <- RWS.asks $ fromMaybe 0 . HashMap.lookup lbl
  RWS.modify $ HashSet.insert lbl
  case t of
    O.ValueType ->
      if visited
        then pure $ O.ValueRef lbl
        else do
          unlinked <- visitValue linked
          if count < 2
            then pure unlinked
            else do
              RWS.tell $ pure $ O.LabelledObject lbl t unlinked
              pure $ O.ValueRef lbl
    O.ObservationType ->
      if visited
        then pure $ O.ObservationRef lbl
        else do
          unlinked <- visitObservation linked
          if count < 2
            then pure unlinked
            else do
              RWS.tell $ pure $ O.LabelledObject lbl t unlinked
              pure $ O.ObservationRef lbl
    O.ContractType ->
      if visited
        then pure $ O.ContractRef lbl
        else do
          unlinked <- visitContract linked
          if count < 2
            then pure unlinked
            else do
              RWS.tell $ pure $ O.LabelledObject lbl t unlinked
              pure $ O.ContractRef lbl
    O.PartyType ->
      if visited
        then pure $ O.PartyRef lbl
        else do
          let unlinked = O.fromCoreParty linked
          if count < 2
            then pure unlinked
            else do
              RWS.tell $ pure $ O.LabelledObject lbl t unlinked
              pure $ O.PartyRef lbl
    O.TokenType ->
      if visited
        then pure $ O.TokenRef lbl
        else do
          let unlinked = O.fromCoreToken linked
          if count < 2
            then pure unlinked
            else do
              RWS.tell $ pure $ O.LabelledObject lbl t unlinked
              pure $ O.TokenRef lbl
    O.ActionType ->
      if visited
        then pure $ O.ActionRef lbl
        else do
          unlinked <- visitAction linked
          if count < 2
            then pure unlinked
            else do
              RWS.tell $ pure $ O.LabelledObject lbl t unlinked
              pure $ O.ActionRef lbl

toLinkedObject :: O.ObjectType a -> Linked a -> LinkedObject
toLinkedObject = \case
  O.ValueType -> LinkedValue
  O.ObservationType -> LinkedObservation
  O.ContractType -> LinkedContract
  O.PartyType -> LinkedParty
  O.TokenType -> LinkedToken
  O.ActionType -> LinkedAction

visitValue :: C.Value C.Observation -> RWS (HashMap O.Label Int) (DList O.LabelledObject) (HashSet O.Label) O.Value
visitValue = \case
  C.AvailableMoney account token ->
    O.AvailableMoney <$> visit O.PartyType account <*> visit O.TokenType token
  C.NegValue a -> O.NegValue <$> visit O.ValueType a
  C.AddValue a b -> on (liftA2 O.AddValue) (visit O.ValueType) a b
  C.SubValue a b -> on (liftA2 O.SubValue) (visit O.ValueType) a b
  C.MulValue a b -> on (liftA2 O.MulValue) (visit O.ValueType) a b
  C.DivValue a b -> on (liftA2 O.DivValue) (visit O.ValueType) a b
  C.ChoiceValue (C.ChoiceId name owner) ->
    O.ChoiceValue . O.ChoiceId (T.decodeUtf8 $ PV2.fromBuiltin name) <$> visit O.PartyType owner
  C.Cond c a b -> O.Cond <$> visit O.ObservationType c <*> visit O.ValueType a <*> visit O.ValueType b
  c -> pure $ O.fromCoreValue c

visitObservation :: C.Observation -> RWS (HashMap O.Label Int) (DList O.LabelledObject) (HashSet O.Label) O.Observation
visitObservation = \case
  C.AndObs a b -> on (liftA2 O.AndObs) (visit O.ObservationType) a b
  C.OrObs a b -> on (liftA2 O.OrObs) (visit O.ObservationType) a b
  C.NotObs a -> O.NotObs <$> visit O.ObservationType a
  C.ChoseSomething (C.ChoiceId name owner) ->
    O.ChoseSomething . O.ChoiceId (T.decodeUtf8 $ PV2.fromBuiltin name) <$> visit O.PartyType owner
  C.ValueGE a b -> on (liftA2 O.ValueGE) (visit O.ValueType) a b
  C.ValueGT a b -> on (liftA2 O.ValueGT) (visit O.ValueType) a b
  C.ValueLE a b -> on (liftA2 O.ValueLE) (visit O.ValueType) a b
  C.ValueLT a b -> on (liftA2 O.ValueLT) (visit O.ValueType) a b
  C.ValueEQ a b -> on (liftA2 O.ValueEQ) (visit O.ValueType) a b
  C.TrueObs -> pure O.TrueObs
  C.FalseObs -> pure O.FalseObs

visitContract :: C.Contract -> RWS (HashMap O.Label Int) (DList O.LabelledObject) (HashSet O.Label) O.Contract
visitContract = \case
  C.Close -> pure O.Close
  C.Pay a (C.Party p) t v c ->
    O.Pay
      <$> visit O.PartyType a
      <*> (O.Party <$> visit O.PartyType p)
      <*> visit O.TokenType t
      <*> visit O.ValueType v
      <*> visit O.ContractType c
  C.Pay a (C.Account p) t v c ->
    O.Pay
      <$> visit O.PartyType a
      <*> (O.Account <$> visit O.PartyType p)
      <*> visit O.TokenType t
      <*> visit O.ValueType v
      <*> visit O.ContractType c
  C.If obs c1 c2 ->
    O.If <$> visit O.ObservationType obs <*> visit O.ContractType c1 <*> visit O.ContractType c2
  C.When cases t c ->
    O.When <$> traverse visitCase cases <*> pure (O.fromCoreTimeout t) <*> visit O.ContractType c
  C.Let vi v c -> O.Let (O.fromCoreValueId vi) <$> visit O.ValueType v <*> visit O.ContractType c
  C.Assert obs c -> O.Assert <$> visit O.ObservationType obs <*> visit O.ContractType c
  where
    visitCase :: C.Case C.Contract -> RWS (HashMap O.Label Int) (DList O.LabelledObject) (HashSet O.Label) O.Case
    visitCase = \case
      C.Case a c -> O.Case <$> visit O.ActionType a <*> visit O.ContractType c
      C.MerkleizedCase a hash -> O.MerkleizedCase <$> visit O.ActionType a <*> pure (O.fromCoreContractHash hash)

visitAction :: C.Action -> RWS (HashMap O.Label Int) (DList O.LabelledObject) (HashSet O.Label) O.Action
visitAction = \case
  C.Deposit a p t v ->
    O.Deposit
      <$> visit O.PartyType a
      <*> visit O.PartyType p
      <*> visit O.TokenType t
      <*> visit O.ValueType v
  C.Choice (C.ChoiceId name owner) bounds ->
    O.Choice . O.ChoiceId (T.decodeUtf8 $ PV2.fromBuiltin name)
      <$> visit O.PartyType owner
      <*> pure (O.fromCoreBound <$> bounds)
  C.Notify obs -> O.Notify <$> visit O.ObservationType obs

countOccurrences :: LinkedObject -> State (HashMap O.Label Int) ()
countOccurrences obj = do
  let label = mkLabel obj
  newCount <- state \counts -> case HashMap.lookup label counts of
    Nothing -> (1, HashMap.insert label 1 counts)
    Just i -> (i + 1, HashMap.insert label (i + 1) counts)
  when (newCount == 1) do
    traverseOf_ linkedChildren countOccurrences obj

mkLabel :: LinkedObject -> O.Label
mkLabel = \case
  LinkedValue a -> hashLabel "value" a
  LinkedObservation a -> hashLabel "observation" a
  LinkedContract a -> hashLabel "contract" a
  LinkedParty a -> hashLabel "party" a
  LinkedToken a -> hashLabel "token" a
  LinkedAction a -> hashLabel "action" a

fromLinkedObject :: O.Label -> LinkedObject -> O.LabelledObject
fromLinkedObject lbl = \case
  LinkedValue a -> O.LabelledObject lbl O.ValueType $ O.fromCoreValue a
  LinkedObservation a -> O.LabelledObject lbl O.ObservationType $ O.fromCoreObservation a
  LinkedContract a -> O.LabelledObject lbl O.ContractType $ O.fromCoreContract a
  LinkedParty a -> O.LabelledObject lbl O.PartyType $ O.fromCoreParty a
  LinkedToken a -> O.LabelledObject lbl O.TokenType $ O.fromCoreToken a
  LinkedAction a -> O.LabelledObject lbl O.ActionType $ O.fromCoreAction a

hashLabel :: (ToData a) => Text -> a -> O.Label
hashLabel prefix a =
  O.Label $ prefix <> "-" <> extractBase16 (encodeBase16 (fromBuiltin $ dataHash a))

linkedChildren :: Fold LinkedObject LinkedObject
linkedChildren = folding \case
  LinkedValue a -> valueChildren a
  LinkedObservation a -> observationChildren a
  LinkedContract a -> contractChildren a
  LinkedParty _ -> []
  LinkedToken _ -> []
  LinkedAction a -> actionChildren a
  where
    valueChildren :: C.Value C.Observation -> [LinkedObject]
    valueChildren = \case
      C.AvailableMoney account token -> [LinkedParty account, LinkedToken token]
      C.NegValue a -> [LinkedValue a]
      C.AddValue a b -> [LinkedValue a, LinkedValue b]
      C.SubValue a b -> [LinkedValue a, LinkedValue b]
      C.MulValue a b -> [LinkedValue a, LinkedValue b]
      C.DivValue a b -> [LinkedValue a, LinkedValue b]
      C.ChoiceValue (C.ChoiceId _ owner) -> [LinkedParty owner]
      C.Cond c a b -> [LinkedObservation c, LinkedValue a, LinkedValue b]
      _ -> []

    observationChildren :: C.Observation -> [LinkedObject]
    observationChildren = \case
      C.AndObs a b -> [LinkedObservation a, LinkedObservation b]
      C.OrObs a b -> [LinkedObservation a, LinkedObservation b]
      C.NotObs a -> [LinkedObservation a]
      C.ChoseSomething (C.ChoiceId _ owner) -> [LinkedParty owner]
      C.ValueGE a b -> [LinkedValue a, LinkedValue b]
      C.ValueGT a b -> [LinkedValue a, LinkedValue b]
      C.ValueLE a b -> [LinkedValue a, LinkedValue b]
      C.ValueLT a b -> [LinkedValue a, LinkedValue b]
      C.ValueEQ a b -> [LinkedValue a, LinkedValue b]
      C.TrueObs -> []
      C.FalseObs -> []

    contractChildren :: C.Contract -> [LinkedObject]
    contractChildren = \case
      C.Close -> []
      C.Pay a (C.Party p) t v c -> [LinkedParty a, LinkedParty p, LinkedToken t, LinkedValue v, LinkedContract c]
      C.Pay a (C.Account p) t v c -> [LinkedParty a, LinkedParty p, LinkedToken t, LinkedValue v, LinkedContract c]
      C.If obs c1 c2 -> [LinkedObservation obs, LinkedContract c1, LinkedContract c2]
      C.When cases _ c -> (caseChildren =<< cases) <> [LinkedContract c]
      C.Let _ v c -> [LinkedValue v, LinkedContract c]
      C.Assert obs c -> [LinkedObservation obs, LinkedContract c]
      where
        caseChildren :: C.Case C.Contract -> [LinkedObject]
        caseChildren = \case
          C.Case a c -> [LinkedAction a, LinkedContract c]
          C.MerkleizedCase a _ -> [LinkedAction a]

    actionChildren :: C.Action -> [LinkedObject]
    actionChildren = \case
      C.Deposit a p t v -> [LinkedParty a, LinkedParty p, LinkedToken t, LinkedValue v]
      C.Choice (C.ChoiceId _ owner) _ -> [LinkedParty owner]
      C.Notify obs -> [LinkedObservation obs]

makePrisms ''LinkError
makePrisms ''LinkedObject
