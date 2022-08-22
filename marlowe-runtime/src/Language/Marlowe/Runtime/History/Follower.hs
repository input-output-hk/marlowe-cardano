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
import Data.Foldable (asum, find, for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
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
                                          MarloweVersionTag (..), PayoutDatum, SomeMarloweVersion (..),
                                          Transaction (..), TransactionOutput (..), TransactionScriptOutput (..),
                                          fromChainDatum, fromChainRedeemer)
import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))

data CreateStep v = CreateStep
  { datum         :: Datum v
  , scriptAddress :: Chain.Address
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
  , getMarloweVersion  :: ScriptHash -> Maybe SomeMarloweVersion
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
  | ExtractMarloweTransactionFailed ExtractMarloweTransactionError
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
              let scriptUTxO = unContractId contractId
              let payoutUTxOs = mempty
              let prevDatum = datum
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
  { version       :: MarloweVersion v
  , create        :: CreateStep v
  , contractId    :: ContractId
  , changesVar    :: TVar (ContractChanges v)
  , prevDatum     :: Datum v
  , scriptUTxO    :: TxOutRef
  , payoutUTxOs   :: Set TxOutRef
  , scriptAddress :: Chain.Address
  , slotConfig    :: SlotConfig
  }

exctractCreation :: FollowerDependencies -> Chain.Transaction -> Either ExtractCreationError SomeCreateStep
exctractCreation FollowerDependencies{..} tx@Chain.Transaction{inputs, validityRange} = do
  Chain.TransactionOutput{ address = scriptAddress, datum = mdatum } <-
    getOutput (txIx $ unContractId contractId) tx
  scriptHash <- getScriptHash scriptAddress
  SomeMarloweVersion version <- note InvalidScriptHash $ getMarloweVersion scriptHash
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
  :: FollowerState v
  -> IO (ClientStIdle Move ChainPoint ChainPoint IO (Either ContractHistoryError ()))
followContract state@FollowerState{..} = do
  let move = FindConsumingTx scriptUTxO
  pure $ SendMsgQueryNext move (followNext state) (pure (followNext state))

followNext
  :: FollowerState v
  -> ClientStNext Move Chain.UTxOError Chain.Transaction ChainPoint ChainPoint IO (Either ContractHistoryError ())
followNext state@FollowerState{..} = ClientStNext
  { recvMsgQueryRejected = \err _ -> failWith $ FollowScriptUTxOFailed err
  , recvMsgRollForward = \tx point _ -> case point of
      Genesis -> error "transaction detected at Genesis"
      At blockHeader -> case extractMarloweTransaction version prevDatum slotConfig contractId scriptAddress scriptUTxO blockHeader tx of
        Left err -> failWith $ ExtractMarloweTransactionFailed err
        Right marloweTx@Transaction{output} -> do
          let changes = mempty { steps = Map.singleton blockHeader [ApplyTransaction marloweTx] }
          atomically $ modifyTVar changesVar (<> changes)
          let TransactionOutput{..} = output
          case scriptOutput of
            Nothing -> pure $ SendMsgDone $ Right ()
            Just TransactionScriptOutput{..} -> do
              -- TODO read payouts to payout validator
              followContract state { scriptUTxO = utxo, prevDatum = datum }
  , recvMsgRollBackward = \_ _ -> error "not implemented"
  }

extractMarloweTransaction
  :: MarloweVersion v
  -> Datum v
  -> SlotConfig
  -> ContractId
  -> Chain.Address
  -> TxOutRef
  -> BlockHeader
  -> Chain.Transaction
  -> Either ExtractMarloweTransactionError (Transaction v)
extractMarloweTransaction version prevDatum slotConfig contractId scriptAddress consumedUTxO blockHeader Chain.Transaction{..} = do
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
  let payouts = [] -- TODO
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
