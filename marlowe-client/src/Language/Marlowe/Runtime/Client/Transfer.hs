-- | Helper clients that perform common contract transfer tasks.
module Language.Marlowe.Runtime.Client.Transfer (
  importBundle,
  importIncremental,
  exportContract,
  exportIncremental,
  BundlePart (..),
) where

import Data.Map (Map)
import Language.Marlowe.Object.Types
import Language.Marlowe.Protocol.Transfer.Client
import Language.Marlowe.Protocol.Transfer.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Numeric.Natural (Natural)
import Pipes (Pipe, Producer, await, yield)

-- | Imports a single object bundle into the Runtime. It will incrementally link the bundle, merkleize the contracts, and
-- save them to the store. Returns a mapping of the original contract labels to their store hashes.
importBundle :: (Applicative m) => ObjectBundle -> MarloweTransferClient m (Either ImportError (Map Label DatumHash))
importBundle bundle =
  MarloweTransferClient $
    pure $
      SendMsgStartImport $
        SendMsgUpload
          bundle
          ClientStUpload
            { recvMsgUploadFailed = pure . SendMsgDone . Left
            , recvMsgUploaded = pure . SendMsgImported . SendMsgDone . Right
            }

-- | A data structure that carries an object bundle and an indication of whether to expect more bundles.
data BundlePart
  = IntermediatePart ObjectBundle
  | FinalPart ObjectBundle

-- | Streams a multi-part object bundle into the Runtime. It will link the bundle, merkleize the contracts, and
-- save them to the store. Yields mappings of the original contract labels to their store hashes. sending it a FinalPart
-- finalizes the import, and is necessary to actually import the bundle. If no FinalPart is sent, nothing will be
-- imported. The final mapping will be returned in the result, and not yielded.
importIncremental
  :: (Functor m)
  => MarloweTransferClient (Pipe BundlePart (Map Label DatumHash) m) (Either ImportError (Map Label DatumHash))
importIncremental = MarloweTransferClient $ SendMsgStartImport <$> upload
  where
    upload = do
      part <- await
      case part of
        FinalPart bundle ->
          pure $
            SendMsgUpload
              bundle
              ClientStUpload
                { recvMsgUploadFailed = pure . SendMsgDone . Left
                , recvMsgUploaded = \hashes -> pure $ SendMsgImported $ SendMsgDone $ Right hashes
                }
        IntermediatePart bundle ->
          pure $
            SendMsgUpload
              bundle
              ClientStUpload
                { recvMsgUploadFailed = pure . SendMsgDone . Left
                , recvMsgUploaded = \hashes -> do
                    yield hashes
                    upload
                }

-- | Exports a contract from the runtime as a single object bundle. The first argument controls the batch size.
exportContract :: (Applicative m) => Natural -> DatumHash -> MarloweTransferClient m (Maybe ObjectBundle)
exportContract batchSize hash =
  MarloweTransferClient $
    pure $
      SendMsgRequestExport
        hash
        ClientStExport
          { recvMsgStartExport = do
              let downloadLoop acc =
                    SendMsgDownload
                      batchSize
                      ClientStDownload
                        { recvMsgDownloaded = \(ObjectBundle bundle) -> pure $ downloadLoop $ acc <> bundle
                        , recvMsgExported = pure $ SendMsgDone $ Just $ ObjectBundle acc
                        }
              pure $ downloadLoop []
          , recvMsgContractNotFound = pure $ SendMsgDone Nothing
          }

-- | Streams a contract from the runtime as a multi-part object bundle. The first argument controls the batch size of the
-- bundles.
exportIncremental :: (Functor m) => Natural -> DatumHash -> MarloweTransferClient (Producer ObjectBundle m) Bool
exportIncremental batchSize hash =
  MarloweTransferClient $
    pure $
      SendMsgRequestExport
        hash
        ClientStExport
          { recvMsgStartExport = do
              let downloadLoop =
                    SendMsgDownload
                      batchSize
                      ClientStDownload
                        { recvMsgDownloaded = \bundle -> do
                            yield bundle
                            pure downloadLoop
                        , recvMsgExported = pure $ SendMsgDone True
                        }
              pure downloadLoop
          , recvMsgContractNotFound = pure $ SendMsgDone False
          }
