{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Marlowe.Runtime.History.Follower
  ( ContractChanges(..)
  , Follower(..)
  , FollowerDependencies(..)
  , SomeContractChanges(..)
  , applyRollback
  , isEmptyChanges
  , mkFollower
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM
  (STM, TVar, atomically, modifyTVar, newEmptyTMVar, newTVar, readTVar, takeTMVar, tryPutTMVar, tryTakeTMVar, writeTVar)
import Control.Monad (guard, mfilter, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, runWriterT, tell)
import Data.Bifunctor (first)
import Data.Foldable (asum, find, for_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Data.Void (Void, absurd)
import GHC.Show (showSpace)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api
  ( BlockHeader
  , ChainPoint
  , ChainSeekClient(..)
  , ClientStHandshake(..)
  , ClientStIdle(..)
  , ClientStInit(..)
  , ClientStNext(..)
  , Move(..)
  , RuntimeChainSeekClient
  , ScriptHash(..)
  , SlotConfig
  , TxOutRef(..)
  , UTxOError
  , WithGenesis(..)
  , isAfter
  , moveSchema
  , slotToUTCTime
  )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.AddressRegistry (MarloweScriptAddresses(..), ScriptAddressInfo(..))
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , Datum
  , IsMarloweVersion(Redeemer)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , SomeMarloweVersion(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  , fromChainDatum
  , fromChainPayoutDatum
  , fromChainRedeemer
  )
import Language.Marlowe.Runtime.History.Api
import Plutus.V1.Ledger.Api (POSIXTime(POSIXTime))

data ContractChanges v = ContractChanges
  { steps      :: Map Chain.BlockHeader [ContractStep v]
  , create     :: Maybe (BlockHeader, CreateStep v)
  , rollbackTo :: Maybe ChainPoint
  }

deriving instance Show (ContractChanges 'V1)
deriving instance Eq (ContractChanges 'V1)

data SomeContractChanges = forall v. SomeContractChanges (MarloweVersion v) (ContractChanges v)

instance Show SomeContractChanges where
  showsPrec p (SomeContractChanges version changes) =
    showParen (p >= 11)
      ( showString "SomeContractChanges"
      . showSpace
      . showsPrec 11 version
      . showSpace
      . case version of
          MarloweV1 -> showsPrec 11 changes
      )

instance Eq SomeContractChanges where
  SomeContractChanges v1 c1 == SomeContractChanges v2 c2 = case (v1, v2) of
    (MarloweV1, MarloweV1) -> c1 == c2

instance Semigroup (ContractChanges v) where
  c1@ContractChanges{create = create1} <> ContractChanges{..} =
    c1' { steps = Map.unionWith (<>) steps1 steps, create = create1 <|> create }
    where
      c1'@ContractChanges{steps=steps1} = maybe c1 (flip applyRollback c1) rollbackTo

instance Monoid (ContractChanges v) where
  mempty = ContractChanges Map.empty Nothing Nothing

isEmptyChanges :: SomeContractChanges -> Bool
isEmptyChanges (SomeContractChanges _ (ContractChanges steps Nothing Nothing)) = Map.null steps
isEmptyChanges _                                                               = False

applyRollback :: ChainPoint -> ContractChanges v -> ContractChanges v
applyRollback Genesis _ = ContractChanges mempty Nothing $ Just Genesis
applyRollback (At blockHeader@Chain.BlockHeader{slotNo}) ContractChanges{..} = ContractChanges
  { steps = steps'
  , create = mfilter (isNotRolledBack . fst) create
  , rollbackTo = asum
      [ guard (Map.null steps') *> (min (Just (At blockHeader)) rollbackTo <|> Just (At blockHeader))
      , rollbackTo
      ]
  }
  where
    steps' = Map.filterWithKey (const . isNotRolledBack) steps
    isNotRolledBack = not . isAfter slotNo

data FollowerDependencies = FollowerDependencies
  { contractId         :: ContractId
  , getMarloweVersion  :: ScriptHash -> Maybe (SomeMarloweVersion, MarloweScriptAddresses)
  , connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  , slotConfig         :: SlotConfig
  , securityParameter  :: Int
  }

data Follower = Follower
  { runFollower    :: IO (Either ContractHistoryError ())
  , status         :: STM FollowerStatus
  , changes        :: STM (Maybe SomeContractChanges)
  , cancelFollower :: STM ()
  }

data ContractChangesTVar v = ContractChangesTVar (MarloweVersion v) (TVar (ContractChanges v))
data SomeContractChangesTVar = forall v. SomeContractChangesTVar (ContractChangesTVar v)

mkFollower :: FollowerDependencies -> STM Follower
mkFollower deps@FollowerDependencies{..} = do
  someChangesVar <- newTVar Nothing
  statusVar <- newTVar Pending
  cancelled <- newEmptyTMVar
  let
    stInit = SendMsgRequestHandshake moveSchema handshake
    handshake = ClientStHandshake
      { recvMsgHandshakeRejected = \_ -> pure $ Left HansdshakeFailed
      , recvMsgHandshakeConfirmed = findContract
      }

    findContract = do
      let move = FindTx $ txId $ unContractId contractId
      pure $ SendMsgQueryNext move handleContract (pure handleContract)

    handleContract = ClientStNext
      { recvMsgQueryRejected = \err _ -> failWith $ FindTxFailed err
      , recvMsgRollForward = \tx point _ -> case point of
          Genesis -> error "transaction detected at Genesis"
          At blockHeader -> case extractCreation deps tx of
            Left err ->
              failWith $ ExtractContractFailed err
            Right (SomeCreateStep version create@CreateStep{..}) -> do
              changesVar <- atomically do
                changesVar <- newTVar $ ContractChanges
                  { steps = Map.empty
                  , create = Just (blockHeader, create)
                  , rollbackTo = Nothing
                  }
                writeTVar someChangesVar
                  $ Just
                  $ SomeContractChangesTVar
                  $ ContractChangesTVar version changesVar
                writeTVar statusVar $ Following $ SomeMarloweVersion version
                pure changesVar
              let payouts = mempty
              let scriptOutput = TransactionScriptOutput (unContractId contractId) datum
              let previousState = Nothing
              followContract blockHeader FollowerContext{..} FollowerState{..}
      , recvMsgRollBackward = \_ _ -> error "Rolled back from genesis"
      }

  pure Follower
    { runFollower = do
        _ <- atomically $ tryTakeTMVar cancelled
        runConcurrently $ asum $ Concurrently <$>
          [ atomically $ Right <$> takeTMVar cancelled
          , do
              result <- connectToChainSeek $ ChainSeekClient $ pure stInit
              atomically $ writeTVar statusVar case result of
                Left err      -> Failed err
                Right version -> Finished version
              pure $ () <$ result
          ]
    , changes = do
        mChangesVar <- readTVar someChangesVar
        for mChangesVar \(SomeContractChangesTVar (ContractChangesTVar version changesVar)) -> do
          changes <- readTVar changesVar
          writeTVar changesVar mempty
          pure $ SomeContractChanges version changes
    , status = readTVar statusVar
    , cancelFollower = void $ tryPutTMVar cancelled ()
    }

data FollowerContext v = FollowerContext
  { version             :: MarloweVersion v
  , create              :: CreateStep v
  , contractId          :: ContractId
  , changesVar          :: TVar (ContractChanges v)
  , statusVar           :: TVar FollowerStatus
  , scriptAddress       :: Chain.Address
  , payoutValidatorHash :: ScriptHash
  , slotConfig          :: SlotConfig
  , securityParameter   :: Int
  }

data PreviousState a
  = Retained BlockHeader a
  | Truncated
  deriving (Functor)

data FollowerState v = FollowerState
  { payouts       :: Map Chain.TxOutRef (Payout v)
  , scriptOutput  :: TransactionScriptOutput v
  , previousState :: Maybe (PreviousState (FollowerState v))
  }

data ClosedPreviousState v
  = ClosedPreviousOpen (FollowerState v)
  | ClosedPreviousClosed (FollowerStateClosed v)

data FollowerStateClosed v = FollowerStateClosed
  { payouts       :: Map Chain.TxOutRef (Payout v)
  , previousState :: PreviousState (ClosedPreviousState v)
  }

extractCreation :: FollowerDependencies -> Chain.Transaction -> Either ExtractCreationError SomeCreateStep
extractCreation FollowerDependencies{..} tx@Chain.Transaction{inputs, validityRange} = do
  Chain.TransactionOutput{ address = scriptAddress, datum = mdatum } <-
    getOutput (txIx $ unContractId contractId) tx
  marloweScriptHash <- getScriptHash scriptAddress
  (SomeMarloweVersion version, MarloweScriptAddresses{..}) <- note InvalidScriptHash $ getMarloweVersion marloweScriptHash
  let
    payoutValidatorHash = scriptHash payoutScriptAddress
    wouldCloseContract' mdatum' mredeemer = fromMaybe False do
      datum <- fromChainDatum version =<< mdatum'
      redeemer <- fromChainRedeemer version =<< mredeemer
      (minSlot, maxSlot) <- case validityRange of
        Chain.MinMaxBound minSlot maxSlot -> Just (minSlot, maxSlot)
        _                                 -> Nothing
      let validityLowerBound = slotToUTCTime slotConfig minSlot
      let validityUpperBound = slotToUTCTime slotConfig maxSlot
      pure $ wouldCloseContract version validityLowerBound validityUpperBound redeemer datum
  for_ inputs \Chain.TransactionInput{..} ->
    when (isScriptAddress marloweScriptHash address && not (wouldCloseContract' datumBytes redeemer)) $ Left NotCreationTransaction
  txDatum <- note NoCreateDatum mdatum
  datum <- note InvalidCreateDatum $ fromChainDatum version txDatum
  pure $ SomeCreateStep version CreateStep{..}

getScriptHash :: Chain.Address -> Either ExtractCreationError ScriptHash
getScriptHash address = do
  credential <- note ByronAddress $ Chain.paymentCredential address
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

sendMsgQueryNext
  :: FollowerContext v
  -> query err result
  -> ClientStNext query err result point tip IO a
  -> ClientStIdle query point tip IO a
sendMsgQueryNext FollowerContext{..} move next =
  SendMsgQueryNext move next do
    atomically $ writeTVar statusVar $ Waiting $ SomeMarloweVersion version
    pure next

followContract
  :: BlockHeader
  -> FollowerContext v
  -> FollowerState v
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
followContract blockHeader context state@FollowerState{..} = do
  let move = FindConsumingTxs $ Set.insert scriptUTxO $ Map.keysSet payouts
  pure $ sendMsgQueryNext context move $ followNext blockHeader context state
  where
    scriptUTxO = let TransactionScriptOutput{..} = scriptOutput in utxo

followNext
  :: forall v
   . BlockHeader
  -> FollowerContext v
  -> FollowerState v
  -> ClientStNext Move (Map Chain.TxOutRef Chain.UTxOError) (Map Chain.TxOutRef Chain.Transaction) ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion)
followNext previousBlockHeader context@FollowerContext{..} state@FollowerState{..} = ClientStNext
  { recvMsgQueryRejected = \err _ -> failWith case Map.lookup scriptUTxO err of
      Nothing   -> FollowPayoutUTxOsFailed err
      Just err' -> FollowScriptUTxOFailed err'
  , recvMsgRollForward = \txs point _ -> case point of
      Genesis -> error "transaction detected at Genesis"
      At blockHeader -> do
        let result = runWriterT (processTxs blockHeader context state txs)
        case result of
          Left err -> failWith err
          Right (mOutput, changes) -> do
            let
              followContract' :: FollowerState v -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
              followContract' state'@FollowerState{payouts = payouts'} = followContract blockHeader context state'
                { payouts = Map.withoutKeys payouts' $ Map.keysSet txs
                , previousState = Just $ truncateFollowerState securityParameter blockHeader $ Retained previousBlockHeader state
                }
            atomically $ modifyTVar changesVar (<> changes)
            case mOutput of
              Nothing -> followContract' state
              Just (TransactionOutput newPayouts mScriptOutput) -> case mScriptOutput of
                Nothing            -> followContractClosed blockHeader context $ FollowerStateClosed
                  { payouts = Map.withoutKeys payouts $ Map.keysSet txs
                  , previousState = truncateFollowerStateClosed securityParameter blockHeader $ Retained previousBlockHeader $ ClosedPreviousOpen state
                  }
                Just scriptOutput' -> followContract' state { scriptOutput = scriptOutput', payouts = Map.union payouts newPayouts }
  , recvMsgRollBackward = \point _ -> do
      next <- case point of
        Genesis        -> failWith CreateTxRolledBack
        At blockHeader -> rollbackPreviousState blockHeader context previousState
      atomically $ modifyTVar changesVar $ applyRollback point
      pure next
  }
  where
    scriptUTxO = let TransactionScriptOutput{..} = scriptOutput in utxo

followContractClosed
  :: BlockHeader
  -> FollowerContext v
  -> FollowerStateClosed v
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
followContractClosed blockHeader context@FollowerContext{..} state@FollowerStateClosed{..}
  | Map.null payouts = pure
    $ sendMsgQueryNext context (AdvanceBlocks $ fromIntegral securityParameter)
    $ followNextRetire context state
  | otherwise = pure
    $ sendMsgQueryNext context (FindConsumingTxs $ Map.keysSet payouts)
    $ followNextPayout blockHeader context state

followNextRetire
  :: FollowerContext v
  -> FollowerStateClosed v
  -> ClientStNext Move Void () ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion)
followNextRetire context@FollowerContext{..} state = ClientStNext
  { recvMsgQueryRejected = absurd
  , recvMsgRollForward = \_ _ _ -> pure $ SendMsgDone $ Right $ SomeMarloweVersion version
  , recvMsgRollBackward = \point _ -> followNextHandleRollback point context state
  }

followNextPayout
  :: BlockHeader
  -> FollowerContext v
  -> FollowerStateClosed v
  -> ClientStNext Move (Map TxOutRef UTxOError) (Map TxOutRef Chain.Transaction) ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion)
followNextPayout previousBlockHeader context@FollowerContext{..} state@FollowerStateClosed{..} = ClientStNext
  { recvMsgQueryRejected = \err _ -> failWith $ FollowPayoutUTxOsFailed err
  , recvMsgRollForward = \txs point _ -> case point of
      Genesis        -> error "transaction detected at Genesis"
      At blockHeader -> do
        case execWriterT $ Map.traverseWithKey (processPayout blockHeader payouts) txs of
          Left err -> failWith err
          Right changes -> do
            atomically $ modifyTVar changesVar (<> changes)
            followContractClosed blockHeader context $ state
              { previousState = truncateFollowerStateClosed securityParameter blockHeader $ Retained previousBlockHeader $ ClosedPreviousClosed state
              , payouts = Map.withoutKeys payouts $ Map.keysSet txs
              }
  , recvMsgRollBackward = \point _ -> followNextHandleRollback point context state
  }

followNextHandleRollback
  :: ChainPoint
  -> FollowerContext v
  -> FollowerStateClosed v
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
followNextHandleRollback point context@FollowerContext{..} FollowerStateClosed{..}= do
  next <- case point of
    Genesis        -> failWith CreateTxRolledBack
    At blockHeader -> rollbackPreviousStateClosed blockHeader context previousState
  atomically $ modifyTVar changesVar $ applyRollback point
  pure next

rollbackPreviousState
  :: forall v
   . BlockHeader
  -> FollowerContext v
  -> Maybe (PreviousState (FollowerState v))
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
rollbackPreviousState blockHeader context = \case
  Nothing -> failWith CreateTxRolledBack
  Just Truncated -> error "encountered rollback beyond security parameter"
  Just (Retained blockHeader' state@FollowerState{..}) -> if blockHeader' <= blockHeader
    then do
      followContract blockHeader' context state
    else rollbackPreviousState blockHeader context previousState

rollbackPreviousStateClosed
  :: forall v
   . BlockHeader
  -> FollowerContext v
  -> PreviousState (ClosedPreviousState v)
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
rollbackPreviousStateClosed blockHeader context = \case
  Truncated -> error "encountered rollback beyond security parameter"
  Retained blockHeader' (ClosedPreviousOpen state) -> rollbackPreviousState blockHeader context $ Just $ Retained blockHeader' state
  Retained blockHeader' (ClosedPreviousClosed state@FollowerStateClosed{..}) -> if blockHeader' <= blockHeader
    then followContractClosed blockHeader' context state
    else rollbackPreviousStateClosed blockHeader context previousState

truncateFollowerState :: forall v. Int -> BlockHeader -> PreviousState (FollowerState v) -> PreviousState (FollowerState v)
truncateFollowerState securityParameter blockHeader@Chain.BlockHeader{..} = \case
  Truncated -> Truncated
  Retained previousBlockHeader state@FollowerState{..}
    | isPastSecurityParameter previousBlockHeader -> Truncated
    | otherwise -> Retained @(FollowerState v) previousBlockHeader state { previousState = truncateFollowerState securityParameter blockHeader <$> previousState }
  where
    isPastSecurityParameter Chain.BlockHeader { blockNo = blockNo' } = blockNo - blockNo' > fromIntegral securityParameter

truncateFollowerStateClosed :: forall v. Int -> BlockHeader -> PreviousState (ClosedPreviousState v) -> PreviousState (ClosedPreviousState v)
truncateFollowerStateClosed securityParameter blockHeader@Chain.BlockHeader{..} = \case
  Truncated -> Truncated
  Retained previousBlockHeader previous -> case previous of
    ClosedPreviousOpen previousOpen -> ClosedPreviousOpen <$> truncateFollowerState securityParameter blockHeader (Retained previousBlockHeader previousOpen)
    ClosedPreviousClosed previousClosed@FollowerStateClosed{..}
      | isPastSecurityParameter previousBlockHeader -> Truncated
      | otherwise -> Retained @(ClosedPreviousState v) previousBlockHeader $ ClosedPreviousClosed previousClosed { previousState = truncateFollowerStateClosed securityParameter blockHeader previousState }
  where
    isPastSecurityParameter Chain.BlockHeader { blockNo = blockNo' } = blockNo - blockNo' > fromIntegral securityParameter

processTxs
  :: BlockHeader
  -> FollowerContext v
  -> FollowerState v
  -> Map TxOutRef Chain.Transaction
  -> WriterT (ContractChanges v) (Either ContractHistoryError) (Maybe (TransactionOutput v))
processTxs blockHeader context state@FollowerState{..} txs = do
  void $ Map.traverseWithKey (processPayout blockHeader payouts) $ Map.delete scriptUTxO txs
  traverse (processScriptTx blockHeader context state) $ Map.lookup scriptUTxO txs
  where
    scriptUTxO = let TransactionScriptOutput{..} = scriptOutput in utxo

processPayout
  :: BlockHeader
  -> Map Chain.TxOutRef (Payout v)
  -> TxOutRef
  -> Chain.Transaction
  -> WriterT (ContractChanges v) (Either ContractHistoryError) ()
processPayout blockHeader payouts utxo Chain.Transaction{..} = case Map.lookup utxo payouts of
  Nothing -> lift $ Left $ PayoutUTxONotFound utxo
  Just Payout{..} -> do
    let redeemingTx = txId
    tellStep blockHeader $ RedeemPayout $ RedeemStep{..}

processScriptTx
  :: BlockHeader
  -> FollowerContext v
  -> FollowerState v
  -> Chain.Transaction
  -> WriterT (ContractChanges v) (Either ContractHistoryError) (TransactionOutput v)
processScriptTx blockHeader FollowerContext{..} FollowerState{..} tx = do
  let TransactionScriptOutput utxo prevDatum = scriptOutput
  marloweTx@Transaction{output} <- lift
    $ first ExtractMarloweTransactionFailed
    $ extractMarloweTransaction version prevDatum slotConfig contractId scriptAddress payoutValidatorHash utxo blockHeader tx
  tellStep blockHeader $ ApplyTransaction marloweTx
  pure output

tellStep :: BlockHeader -> ContractStep v -> WriterT (ContractChanges v) (Either ContractHistoryError) ()
tellStep blockHeader step = tell ContractChanges
  { steps = Map.singleton blockHeader [step]
  , create = Nothing
  , rollbackTo = Nothing
  }

extractMarloweTransaction
  :: MarloweVersion v
  -> Datum v
  -> SlotConfig
  -> ContractId
  -> Chain.Address
  -> Chain.ScriptHash
  -> TxOutRef
  -> BlockHeader
  -> Chain.Transaction
  -> Either ExtractMarloweTransactionError (Transaction v)
extractMarloweTransaction version prevDatum slotConfig contractId scriptAddress payoutValidatorHash consumedUTxO blockHeader Chain.Transaction{..} = do
  let transactionId = txId
  Chain.TransactionInput { redeemer = mRedeemer } <-
    note TxInNotFound $ find (consumesUTxO consumedUTxO) inputs
  rawRedeemer <- note NoRedeemer mRedeemer
  redeemer <- note InvalidRedeemer $ fromChainRedeemer version rawRedeemer
  (minSlot, maxSlot) <- case validityRange of
    Chain.MinMaxBound minSlot maxSlot -> pure (minSlot, maxSlot)
    _                                 -> Left InvalidValidityRange
  let validityLowerBound = slotToUTCTime slotConfig minSlot
  let validityUpperBound = slotToUTCTime slotConfig maxSlot
  let wouldClose = wouldCloseContract version validityLowerBound validityUpperBound redeemer prevDatum
  scriptOutput <- runMaybeT do
    guard $ not wouldClose
    (ix, Chain.TransactionOutput{ datum = mDatum }) <-
      hoistMaybe $ find (isToAddress scriptAddress . snd) $ zip [0..] outputs
    lift do
      rawDatum <- note NoTransactionDatum mDatum
      datum <- note InvalidTransactionDatum $ fromChainDatum version rawDatum
      let txIx = Chain.TxIx ix
      let utxo = Chain.TxOutRef{..}
      pure TransactionScriptOutput{..}
  let
    payoutOutputs = Map.filter (isToScriptHash payoutValidatorHash)
      $ Map.fromList
      $ (\(txIx, output) -> (Chain.TxOutRef{..}, output)) <$> zip [0..] outputs
  payouts <- flip Map.traverseWithKey payoutOutputs \txOut Chain.TransactionOutput{datum=mPayoutDatum, assets} -> do
    rawPayoutDatum <- note (NoPayoutDatum txOut) mPayoutDatum
    payoutDatum <- note (InvalidPayoutDatum txOut) $ fromChainPayoutDatum version rawPayoutDatum
    pure $ Payout assets payoutDatum
  let output = TransactionOutput{..}
  pure Transaction{..}

wouldCloseContract :: MarloweVersion v -> UTCTime -> UTCTime -> Redeemer v -> Datum v -> Bool
wouldCloseContract version validityLowerBound validityUpperBound redeemer datum = case version of
  MarloweV1 ->
    let
      utcTimeToPOSIXTime = POSIXTime . round . (* 1000) . utcTimeToPOSIXSeconds
      timeInterval = (utcTimeToPOSIXTime validityLowerBound, utcTimeToPOSIXTime validityUpperBound)
      input = V1.TransactionInput timeInterval redeemer
      V1.MarloweData{..} = datum
    in
      marloweContract == V1.Close || case V1.computeTransaction input marloweState marloweContract of
        V1.TransactionOutput{..} -> txOutContract == V1.Close
        -- TODO log a warning here when adding structured logging.
        _                        -> False


isToScriptHash :: Chain.ScriptHash -> Chain.TransactionOutput -> Bool
isToScriptHash toScriptHash Chain.TransactionOutput{..} = case Chain.paymentCredential address of
  Just (Chain.ScriptCredential hash) -> hash == toScriptHash
  _                                  -> False

isToAddress :: Chain.Address -> Chain.TransactionOutput -> Bool
isToAddress toAddress Chain.TransactionOutput{..} = address == toAddress

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

consumesUTxO :: TxOutRef -> Chain.TransactionInput -> Bool
consumesUTxO TxOutRef{..} Chain.TransactionInput { txId = txInId, txIx = txInIx } =
  txId == txInId && txIx == txInIx

failWith :: ContractHistoryError -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError SomeMarloweVersion))
failWith = pure . SendMsgDone . Left

note :: a -> Maybe b -> Either a b
note e = maybe (Left e) Right
