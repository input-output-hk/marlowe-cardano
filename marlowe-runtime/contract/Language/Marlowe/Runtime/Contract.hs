{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract where

import Control.Arrow (arr)
import Control.Concurrent.Component (Component)
import Control.Concurrent.Component.Probes
import Data.Aeson.Text (encodeToLazyText)
import Data.Set (toList)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Void (absurd)
import Language.Marlowe (TransactionInput(..))
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.LoadServer
import Language.Marlowe.Runtime.Contract.QueryServer
import Language.Marlowe.Runtime.Contract.Store
import Network.Protocol.Connection (Socket)
import Network.Protocol.Query.Server (QueryServer)
import Network.TypedProtocol
import Observe.Event.Render.OpenTelemetry (OTelRendered(..), RenderSelectorOTel)
import OpenTelemetry.Attributes (PrimitiveAttribute(..))
import OpenTelemetry.Trace.Core (SpanKind(..), toAttribute)
import UnliftIO (MonadUnliftIO)

data ContractDependencies n m = ContractDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  }

data MarloweContract m = MarloweContract
  { loadSocket :: Socket MarloweLoadServer m ()
  , querySocket :: Socket (QueryServer ContractRequest) m ()
  , probes :: Probes
  }

contract :: MonadUnliftIO m => Component m (ContractDependencies n m) (MarloweContract m)
contract = arr \ContractDependencies{..} -> MarloweContract
  { loadSocket = loadServer LoadServerDependencies{..}
  , querySocket = queryServer QueryServerDependencies{..}
  , probes = Probes
    { liveness = pure True
    , readiness = pure True
    , startup = pure True
    }
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
        MerkleizeInputsInput TransactionInput{..} ->
          [ ("marlowe.interval_low" , toAttribute $ IntAttribute $ fromIntegral $ fst txInterval)
          , ("marlowe.interval_high" , toAttribute $ IntAttribute $ fromIntegral $ snd txInterval)
          , ("marlowe.contract.initial_inputs", toAttribute $ toStrict . encodeToLazyText <$> txInputs)
          ]
        MerkleizeInputsResult (Left err) ->
          [("error", fromString $ show err)]
        MerkleizeInputsResult (Right TransactionInput{..}) ->
          [("marlowe.inputs", toAttribute $ toStrict . encodeToLazyText <$> txInputs)]
    }

renderContractStagingAreaSelectorOTel :: RenderSelectorOTel ContractStagingAreaSelector
renderContractStagingAreaSelectorOTel = \case
  StageContract -> OTelRendered
    { eventName = "marlowe/contract/stage"
    , eventKind = Internal
    , renderField = \case
        StageContractContract c ->
          [("marlowe.contract", toAttribute $ toStrict $ encodeToLazyText c)]
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
