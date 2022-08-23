{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

module Language.Marlowe.Runtime.History.Follower where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, tell)
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
import GHC.Show (showSpace)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, ChainSeekClient (..), ClientStHandshake (..),
                                               ClientStIdle (..), ClientStInit (..), ClientStNext (..), Move (..),
                                               RuntimeChainSeekClient, ScriptHash (..), SlotConfig, SlotNo (..),
                                               TxError, TxId, TxOutRef (..), UTxOError, WithGenesis (..), isAfter,
                                               schemaVersion1_0, slotToUTCTime)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId (..), Datum, IsMarloweVersion (Redeemer), MarloweVersion (..),
                                          MarloweVersionTag (..), Payout (..), PayoutDatum, SomeMarloweVersion (..),
                                          Transaction (..), TransactionOutput (..), TransactionScriptOutput (..),
                                          fromChainDatum, fromChainPayoutDatum, fromChainRedeemer)
import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))

data CreateStep v = CreateStep
  { datum               :: Datum v
  , scriptAddress       :: Chain.Address
  , payoutValidatorHash :: ScriptHash
  }

deriving instance Show (CreateStep 'V1)
deriving instance Eq (CreateStep 'V1)

data SomeCreateStep = forall v. SomeCreateStep (MarloweVersion v) (CreateStep v)

data RedeemStep v = RedeemStep
  { utxo        :: TxOutRef
  , redeemingTx :: TxId
  , datum       :: PayoutDatum v
  }

deriving instance Show (RedeemStep 'V1)
deriving instance Eq (RedeemStep 'V1)

data ContractStep v
  = Create (CreateStep v)
  | ApplyTransaction (Transaction v)
  | RedeemPayout (RedeemStep v)
  -- TODO add TimeoutElapsed

deriving instance Show (ContractStep 'V1)
deriving instance Eq (ContractStep 'V1)

data ContractChanges v = ContractChanges
  { steps      :: Map BlockHeader [ContractStep v]
  , rollbackTo :: Maybe SlotNo
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
  , getMarloweVersion  :: ScriptHash -> Maybe (SomeMarloweVersion, ScriptHash)
  , connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  , slotConfig         :: SlotConfig
  }

data Follower = Follower
  { runFollower :: IO (Either ContractHistoryError ())
  , changes     :: STM (Maybe SomeContractChanges)
  }

data ContractHistoryError
  = HansdshakeFailed
  | FindTxFailed TxError
  | ExtractContractFailed ExtractCreationError
  | FollowScriptUTxOFailed UTxOError
  | FollowPayoutUTxOsFailed (Map Chain.TxOutRef UTxOError)
  | ExtractMarloweTransactionFailed ExtractMarloweTransactionError
  | PayoutUTxONotFound Chain.TxOutRef
  deriving stock (Show, Eq, Ord)

data ExtractCreationError
  = TxIxNotFound
  | ByronAddress
  | NonScriptAddress
  | InvalidScriptHash
  | NoCreateDatum
  | InvalidCreateDatum
  | NotCreationTransaction
  deriving stock (Show, Eq, Ord)

data ExtractMarloweTransactionError
  = TxInNotFound
  | NoRedeemer
  | InvalidRedeemer
  | NoTransactionDatum
  | InvalidTransactionDatum
  | NoPayoutDatum TxOutRef
  | InvalidPayoutDatum TxOutRef
  | InvalidValidityRange
  deriving stock (Show, Eq, Ord)

data ContractChangesTVar v = ContractChangesTVar (MarloweVersion v) (TVar (ContractChanges v))
data SomeContractChangesTVar = forall v. SomeContractChangesTVar (ContractChangesTVar v)

mkFollower :: FollowerDependencies -> STM Follower
mkFollower deps@FollowerDependencies{..} = do
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
      { recvMsgQueryRejected = \err _ -> failWith $ FindTxFailed err
      , recvMsgRollForward = \tx point _ -> case point of
          Genesis -> error "transaction detected at Genesis"
          At blockHeader -> case exctractCreation deps tx of
            Left err ->
              failWith $ ExtractContractFailed err
            Right (SomeCreateStep version create@CreateStep{..}) -> do
              changesVar <- atomically do
                changesVar <- newTVar mempty { steps = Map.singleton blockHeader [Create create] }
                writeTVar someChangesVar
                  $ Just
                  $ SomeContractChangesTVar
                  $ ContractChangesTVar version changesVar
                pure changesVar
              let payouts = mempty
              let scriptOutput = TransactionScriptOutput (unContractId contractId) datum
              followContract FollowerContext{..} FollowerState{..}
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

data FollowerContext v = FollowerContext
  { version             :: MarloweVersion v
  , create              :: CreateStep v
  , contractId          :: ContractId
  , changesVar          :: TVar (ContractChanges v)
  , scriptAddress       :: Chain.Address
  , payoutValidatorHash :: ScriptHash
  , slotConfig          :: SlotConfig
  }

data FollowerState v = FollowerState
  { payouts      :: Map Chain.TxOutRef (Payout v)
  , scriptOutput :: TransactionScriptOutput v
  }

exctractCreation :: FollowerDependencies -> Chain.Transaction -> Either ExtractCreationError SomeCreateStep
exctractCreation FollowerDependencies{..} tx@Chain.Transaction{inputs, validityRange} = do
  Chain.TransactionOutput{ address = scriptAddress, datum = mdatum } <-
    getOutput (txIx $ unContractId contractId) tx
  scriptHash <- getScriptHash scriptAddress
  (SomeMarloweVersion version, payoutValidatorHash) <-note InvalidScriptHash $ getMarloweVersion scriptHash
  let
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
    when (isScriptAddress scriptHash address && not (wouldCloseContract' datumBytes redeemer)) $ Left NotCreationTransaction
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

followContract
  :: FollowerContext v
  -> FollowerState v
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError ()))
followContract context state@FollowerState{..} = do
  let move = FindConsumingTxs $ Set.insert scriptUTxO $ Map.keysSet payouts
  pure $ SendMsgQueryNext move (followNext context state) (pure (followNext context state))
  where
    scriptUTxO = let TransactionScriptOutput{..} = scriptOutput in utxo

followNext
  :: forall v
   . FollowerContext v
  -> FollowerState v
  -> ClientStNext Move (Map Chain.TxOutRef Chain.UTxOError) (Map Chain.TxOutRef Chain.Transaction) ChainPoint ChainPoint IO (Either ContractHistoryError ())
followNext context@FollowerContext{..} state@FollowerState{..} = ClientStNext
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
              nextState :: FollowerState v -> FollowerState v
              nextState state' = state' { payouts = Map.withoutKeys payouts $ Map.keysSet txs }
            atomically $ modifyTVar changesVar (<> changes)
            case mOutput of
              Nothing -> followContract context $ nextState state
              Just (TransactionOutput newPayouts mScriptOutput) -> case mScriptOutput of
                Nothing            -> pure $ SendMsgDone $ Right ()
                Just scriptOutput' -> followContract context $ nextState $ state { scriptOutput = scriptOutput', payouts = Map.union payouts newPayouts }
  , recvMsgRollBackward = \_ _ -> error "not implemented"
  }
  where
    scriptUTxO = let TransactionScriptOutput{..} = scriptOutput in utxo

processTxs
  :: BlockHeader
  -> FollowerContext v
  -> FollowerState v
  -> Map TxOutRef Chain.Transaction
  -> WriterT (ContractChanges v) (Either ContractHistoryError) (Maybe (TransactionOutput v))
processTxs blockHeader context state@FollowerState{..} txs = do
  void $ Map.traverseWithKey (processPayout blockHeader state) $ Map.delete scriptUTxO txs
  traverse (processScriptTx blockHeader context state) $ Map.lookup scriptUTxO txs
  where
    scriptUTxO = let TransactionScriptOutput{..} = scriptOutput in utxo

processPayout
  :: BlockHeader
  -> FollowerState v
  -> TxOutRef
  -> Chain.Transaction
  -> WriterT (ContractChanges v) (Either ContractHistoryError) ()
processPayout blockHeader  FollowerState{..} utxo Chain.Transaction{..} = case Map.lookup utxo payouts of
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
tellStep blockHeader step = tell mempty { steps = Map.singleton blockHeader [step] }

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

failWith :: ContractHistoryError -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError ()))
failWith = pure . SendMsgDone . Left

note :: a -> Maybe b -> Either a b
note e = maybe (Left e) Right
