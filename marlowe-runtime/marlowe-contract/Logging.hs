{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  where

import Control.Monad.Event.Class
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (Foldable(toList))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Void (absurd)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.LoadServer (LoadServerSelector(..))
import Language.Marlowe.Runtime.Contract.Store
  (ContractStagingAreaSelector(..), ContractStoreSelector(..), MerkleizeInputsField(..), StageContractField(..))
import Network.Protocol.Driver.Trace (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Explicit (injectSelector)
import Observe.Event.Render.OpenTelemetry (OTelRendered(..), RenderSelectorOTel)
import OpenTelemetry.Trace

data RootSelector f where
  ContractStoreSelector :: ContractStoreSelector f -> RootSelector f
  LoadServerSelector :: LoadServerSelector f -> RootSelector f
  MarloweLoadServer :: TcpServerSelector (Handshake MarloweLoad) f -> RootSelector f
  QueryServer :: TcpServerSelector (Handshake (Query ContractRequest)) f -> RootSelector f

instance Inject (TcpServerSelector (Handshake (Query ContractRequest))) RootSelector where
  inject = injectSelector QueryServer

instance Inject (TcpServerSelector (Handshake MarloweLoad)) RootSelector where
  inject = injectSelector MarloweLoadServer

instance Inject ContractStoreSelector RootSelector where
  inject = injectSelector ContractStoreSelector

instance Inject LoadServerSelector RootSelector where
  inject = injectSelector LoadServerSelector

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  MarloweLoadServer sel -> renderTcpServerSelectorOTel sel
  QueryServer sel -> renderTcpServerSelectorOTel sel
  ContractStoreSelector sel -> renderContractStoreSelectorOTel sel
  LoadServerSelector sel -> renderLoadServerSelectorOTel sel

renderLoadServerSelectorOTel :: RenderSelectorOTel LoadServerSelector
renderLoadServerSelectorOTel = \case
  LoadContract -> OTelRendered
    { eventName = "marlowe/contact/load"
    , eventKind = Server
    , renderField = \contract ->
      [("marlowe.contract", toAttribute $ toStrict $ encodeToLazyText contract)]
    }

renderContractStoreSelectorOTel :: RenderSelectorOTel ContractStoreSelector
renderContractStoreSelectorOTel = \case
  CreateContractStagingArea -> OTelRendered
    { eventName = "marlowe/contract/create_staging_area"
    , eventKind = Internal
    , renderField = absurd
    }
  ContractStagingAreaSelector sel -> renderContractStagingAreaSelectorOTel sel
  GetContract hash -> OTelRendered
    { eventName = "marlowe/contract/get_contract " <> read (show hash)
    , eventKind = Client
    , renderField = const [("marlowe.contract.contract_exists", toAttribute True)]
    }
  MerkleizeInputs -> OTelRendered
    { eventName = "marlowe/contract/merkleize_inputs"
    , eventKind = Client
    , renderField = \case
        MerkleizeInputsState state -> [("marlowe.state", toAttribute $ toStrict $ encodeToLazyText state)]
        MerkleizeInputsContractHash hash -> [("marlowe.contract_hash", fromString $ read $ show hash)]
        MerkleizeInputsInterval (lo, hi) ->
          [ ("marlowe.interval_low" , toAttribute $ IntAttribute $ fromIntegral lo)
          , ("marlowe.interval_high" , toAttribute $ IntAttribute $ fromIntegral hi)
          ]
        MerkleizeInputsInputs inputs ->
          [("marlowe.contract.input_contents", toAttribute $ toStrict . encodeToLazyText <$> inputs)]
        MerkleizeInputsResult (Left err) ->
          [("error", fromString $ show err)]
        MerkleizeInputsResult (Right inputs) ->
          [("marlowe.inputs", toAttribute $ toStrict . encodeToLazyText <$> inputs)]
    }

renderContractStagingAreaSelectorOTel :: RenderSelectorOTel ContractStagingAreaSelector
renderContractStagingAreaSelectorOTel = \case
  StageContract -> OTelRendered
    { eventName = "marlowe/contract/stage"
    , eventKind = Internal
    , renderField = \case
        StageContractContract contract ->
          [("marlowe.contract", toAttribute $ toStrict $ encodeToLazyText contract)]
        StageContractHash hash ->
          [("cardano.datum_hash", fromString $ read $ show hash)]
    }
  Flush -> OTelRendered
    { eventName = "marlowe/contract/flush_staging_area"
    , eventKind = Internal
    , renderField = \hashes ->
        [("marlowe.contract.added_contracts", toAttribute $ T.pack . read . show <$> toList hashes)]
    }
  Commit -> OTelRendered
    { eventName = "marlowe/contract/commit_staging_area"
    , eventKind = Internal
    , renderField = \hashes ->
        [("marlowe.contract.added_contracts", toAttribute $ T.pack . read . show <$> toList hashes)]
    }
  Discard -> OTelRendered
    { eventName = "marlowe/contract/discard_staging_area"
    , eventKind = Internal
    , renderField = absurd
    }
