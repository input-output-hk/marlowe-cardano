-- | Helper clients that perform common contract transfer tasks.
module Language.Marlowe.Runtime.Client.Transfer (
  importBundle,
  importIncremental,
  exportContract,
  exportIncremental,
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

-- | Streams a multi-part object bundle into the Runtime. It will link the bundle, merkleize the contracts, and
-- save them to the store. Yields mappings of the original contract labels to their store hashes.
importIncremental
  :: (Functor m) => MarloweTransferClient (Pipe ObjectBundle (Map Label DatumHash) m) (Maybe ImportError)
importIncremental = MarloweTransferClient $ SendMsgStartImport . upload <$> await
  where
    upload bundle =
      SendMsgUpload
        bundle
        ClientStUpload
          { recvMsgUploadFailed = pure . SendMsgDone . Just
          , recvMsgUploaded = \hashes -> do
              yield hashes
              nextBundle <- await
              pure $ upload nextBundle
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
