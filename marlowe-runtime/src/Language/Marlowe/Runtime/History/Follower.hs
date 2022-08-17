{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Language.Marlowe.Runtime.History.Follower where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad (guard, when)
import Data.Foldable (asum, for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Traversable (for)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, Move (..), RuntimeChainSeekClient, RuntimeClientStIdle,
                                               ScriptHash (..), SlotNo (..), TxError, TxId, TxOutRef (..),
                                               WithGenesis (..), isAfter, schemaVersion1_0)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId (..), Datum, MarloweVersion, PayoutDatum,
                                          SomeMarloweVersion (SomeMarloweVersion), Transaction, datumFromData,
                                          getMarloweVersion)
import Network.Protocol.ChainSeek.Client (ChainSeekClient (..), ClientStHandshake (..), ClientStIdle (..),
                                          ClientStInit (..), ClientStNext (..))

data CreateStep v = CreateStep
  { datum      :: Datum v
  , scriptHash :: ScriptHash
  }

data SomeCreateStep = forall v. SomeCreateStep (MarloweVersion v) (CreateStep v)

data RedeemStep v = RedeemStep
  { utxo        :: TxOutRef
  , redeemingTx :: TxId
  , datum       :: PayoutDatum v
  }

data ContractStep v
  = Create (CreateStep v)
  | Transaction (Transaction v)
  | RedeemPayout (RedeemStep v)
  -- TODO add TimeoutElapsed

data ContractChanges v = ContractChanges
  { steps      :: Map BlockHeader [ContractStep v]
  , rollbackTo :: Maybe SlotNo
  }

data SomeContractChanges = forall v. SomeContractChanges (MarloweVersion v) (ContractChanges v)

instance Semigroup (ContractChanges v) where
  ContractChanges{..} <> c2 = c2' { steps = Map.unionWith (<>) steps2 steps }
    where
      c2'@ContractChanges{steps=steps2} = maybe c2 (flip applyRollback c2) rollbackTo

instance Monoid (ContractChanges v) where
  mempty = ContractChanges Map.empty Nothing

applyRollback :: SlotNo -> ContractChanges v -> ContractChanges v
applyRollback slotNo ContractChanges{..} = ContractChanges
  { steps = steps'
  , rollbackTo = asum
      [ guard (Map.null steps') *> (min (Just slotNo) rollbackTo <|> Just slotNo)
      , rollbackTo
      ]
  }
  where
    steps' = Map.filterWithKey (const . not . isAfter slotNo) steps

data FollowerDependencies = FollowerDependencies
  { contractId         :: ContractId
  , connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  }

data Follower = Follower
  { runFollower :: IO (Either ContractHistoryError ())
  , changes     :: STM (Maybe SomeContractChanges)
  }

data ContractHistoryError
  = HansdshakeFailed
  | FindTxFailed TxError
  | ExtractContractFailed ExtractCreationError
  deriving stock (Show, Eq, Ord)

data ExtractCreationError
  = TxIxNotFound
  | ByronAddress
  | NonScriptAddress
  | InvalidScriptHash
  | NoDatum
  | InvalidDatum
  | NotCreationTransaction
  deriving stock (Show, Eq, Ord)

data ContractChangesTVar v = ContractChangesTVar (MarloweVersion v) (TVar (ContractChanges v))
data SomeContractChangesTVar = forall v. SomeContractChangesTVar (ContractChangesTVar v)

mkFollower :: FollowerDependencies -> STM Follower
mkFollower FollowerDependencies{..} = do
  someChangesVar <- newTVar Nothing
  let
    stInit = SendMsgRequestHandshake schemaVersion1_0 handshake
    handshake = ClientStHandshake
      { recvMsgHandshakeRejected = \_ -> pure $ Left HansdshakeFailed
      , recvMsgHandshakeConfirmed = findContract
      }

    findContract = do
      let move = FindTx $ txId $ unContractId contractId
      pure $ SendMsgQueryNext move handleContract (pure handleContract)

    handleContract = ClientStNext
      { recvMsgQueryRejected = \err _ -> pure $ SendMsgDone $ Left $ FindTxFailed err
      , recvMsgRollForward = \tx point _ -> case point of
          Genesis -> error "transaction detected at Genesis"
          At blockHeader -> case exctractCreation contractId tx of
            Left err ->
              pure $ SendMsgDone $ Left $ ExtractContractFailed err
            Right (SomeCreateStep version create) -> do
              changesVar <- atomically do
                changesVar <- newTVar ContractChanges
                  { steps = Map.singleton blockHeader [Create create]
                  , rollbackTo = Nothing
                  }
                writeTVar someChangesVar
                  $ Just
                  $ SomeContractChangesTVar
                  $ ContractChangesTVar version changesVar
                pure changesVar
              let scriptUTxO = Just $ unContractId contractId
              let payoutUTxOs = mempty
              followContract FollowerState{..}
      , recvMsgRollBackward = \_ _ -> error "Rolled back from genesis"
      }

  pure Follower
    { runFollower = connectToChainSeek $ ChainSeekClient $ pure stInit
    , changes = do
        mChangesVar <- readTVar someChangesVar
        for mChangesVar \(SomeContractChangesTVar (ContractChangesTVar version changesVar)) -> do
          changes <- readTVar changesVar
          writeTVar changesVar mempty
          pure $ SomeContractChanges version changes
    }

data FollowerState v = FollowerState
  { version     :: MarloweVersion v
  , create      :: CreateStep v
  , contractId  :: ContractId
  , changesVar  :: TVar (ContractChanges v)
  , scriptUTxO  :: Maybe TxOutRef
  , payoutUTxOs :: Set TxOutRef
  }

exctractCreation :: ContractId -> Chain.Transaction -> Either ExtractCreationError SomeCreateStep
exctractCreation contractId tx@Chain.Transaction{inputs} = do
  Chain.TransactionOutput{address, datum=mdatum} <- getOutput (txIx $ unContractId contractId) tx
  scriptHash <- getScriptHash address
  for_ inputs \Chain.TransactionInput{address=txInAddress} ->
    when (isScriptAddress scriptHash txInAddress) $ Left NotCreationTransaction
  SomeMarloweVersion version <- maybe (Left InvalidScriptHash) Right $ getMarloweVersion scriptHash
  txDatum <- maybe (Left NoDatum) Right mdatum
  datum <- maybe (Left InvalidDatum) Right $ datumFromData version txDatum
  pure $ SomeCreateStep version CreateStep{..}

getScriptHash :: Chain.Address -> Either ExtractCreationError ScriptHash
getScriptHash address = do
  credential <- maybe (Left ByronAddress) Right $ Chain.paymentCredential address
  case credential of
    Chain.ScriptCredential scriptHash -> pure scriptHash
    _                                 -> Left NonScriptAddress

isScriptAddress :: ScriptHash -> Chain.Address -> Bool
isScriptAddress scriptHash address = getScriptHash address == Right scriptHash

getOutput :: Chain.TxIx -> Chain.Transaction -> Either ExtractCreationError Chain.TransactionOutput
getOutput (Chain.TxIx i) Chain.Transaction{..} = go i outputs
  where
    go _ []        = Left TxIxNotFound
    go 0 (x : _)   = Right x
    go i' (_ : xs) = go (i' - 1) xs

followContract :: FollowerState v -> IO (RuntimeClientStIdle IO (Either ContractHistoryError ()))
followContract _ = pure $ SendMsgDone $ Right ()
