{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

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
import Cardano.Api.Shelley (Address (..), StakeCredential (..), fromShelleyPaymentCredential, fromShelleyStakeReference)
import Cardano.Ledger.BaseTypes (Network (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Credential (Ptr (..))
import qualified Cardano.Ledger.Slot as Ledger
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as C
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as C
import qualified Language.Marlowe.Object.Types as O
import Network.Protocol.Codec.Spec (Variations)
import qualified Plutus.V1.Ledger.Address as PV2
import Plutus.V2.Ledger.Api (toBuiltin)
import qualified Plutus.V2.Ledger.Api as PV2

data LinkError
  = UnknownSymbol O.Label
  | DuplicateLabel O.Label
  | TypeMismatch O.SomeObjectType O.SomeObjectType
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Binary, Variations)

type SymbolTable = HashMap O.Label LinkedObject

data LinkedObject
  = LinkedValue (C.Value C.Observation)
  | LinkedObservation C.Observation
  | LinkedContract C.Contract
  | LinkedParty C.Party
  | LinkedToken C.Token
  | LinkedAction C.Action

linkObject
  :: (Monad m)
  => O.LabelledObject
  -> (LinkedObject -> m LinkedObject)
  -> SymbolTable
  -> m (Either LinkError (LinkedObject, SymbolTable))
linkObject O.LabelledObject{..} transform objects
  | HashMap.member label objects = pure $ Left $ DuplicateLabel label
  | otherwise = do
      let mLinked = case type_ of
            O.ValueType -> LinkedValue <$> linkValue objects value
            O.ObservationType -> LinkedObservation <$> linkObservation objects value
            O.ContractType -> LinkedContract <$> linkContract objects value
            O.PartyType -> LinkedParty <$> linkParty objects value
            O.TokenType -> LinkedToken <$> linkToken objects value
            O.ActionType -> LinkedAction <$> linkAction objects value
      case mLinked of
        Left err -> pure $ Left err
        Right linked -> do
          linked' <- transform linked
          pure $ Right (linked', HashMap.insert label linked' objects)

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
  O.ValueRef label -> resolveReference label objects \case
    LinkedValue value -> Right value
    _ -> Left $ O.SomeObjectType O.ValueType

linkObservation :: SymbolTable -> O.Observation -> Either LinkError C.Observation
linkObservation objects = \case
  O.AndObs a b -> C.AndObs <$> linkObservation objects a <*> linkObservation objects b
  O.OrObs a b -> C.AndObs <$> linkObservation objects a <*> linkObservation objects b
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
  O.ObservationRef label -> resolveReference label objects \case
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
  O.ContractRef label -> resolveReference label objects \case
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
  O.PartyRef label -> resolveReference label objects \case
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
  PaymentCredentialByScript scriptHash -> PV2.ScriptCredential $ toPlutusValidatorHash scriptHash

toPlutusValidatorHash :: ScriptHash -> PV2.ValidatorHash
toPlutusValidatorHash = PV2.ValidatorHash . PV2.toBuiltin . serialiseToRawBytes

toPlutusPubKeyHash :: Hash PaymentKey -> PV2.PubKeyHash
toPlutusPubKeyHash = PV2.PubKeyHash . PV2.toBuiltin . serialiseToRawBytes

toPlutusStaking :: StakeAddressReference -> Maybe PV2.StakingCredential
toPlutusStaking = \case
  NoStakeAddress -> Nothing
  StakeAddressByValue credential -> Just $ PV2.StakingHash $ case credential of
    StakeCredentialByKey keyHash ->
      PV2.PubKeyCredential $ PV2.PubKeyHash $ PV2.toBuiltin $ serialiseToRawBytes keyHash
    StakeCredentialByScript scriptHash -> PV2.ScriptCredential $ toPlutusValidatorHash scriptHash
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
  O.TokenRef label -> resolveReference label objects \case
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
  O.ActionRef label -> resolveReference label objects \case
    LinkedAction value -> Right value
    _ -> Left $ O.SomeObjectType O.ActionType

resolveReference :: O.Label -> SymbolTable -> (LinkedObject -> Either O.SomeObjectType a) -> Either LinkError a
resolveReference label objects handle = case HashMap.lookup label objects of
  Nothing -> Left $ UnknownSymbol label
  Just object -> case handle object of
    Left expected -> Left $ TypeMismatch expected case object of
      LinkedValue _ -> O.SomeObjectType O.ValueType
      LinkedObservation _ -> O.SomeObjectType O.ObservationType
      LinkedContract _ -> O.SomeObjectType O.ContractType
      LinkedParty _ -> O.SomeObjectType O.PartyType
      LinkedToken _ -> O.SomeObjectType O.TokenType
      LinkedAction _ -> O.SomeObjectType O.ActionType
    Right a -> pure a
