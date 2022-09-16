-- | Extraction of Marlowe contracts and history from transaction data.

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Client.History
  where


import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics (MarloweData, TransactionInput)
import Language.Marlowe.Scripts (TypedMarloweValidator, TypedRolePayoutValidator)
import Plutus.Script.Utils.V1.Typed.Scripts (TypedScriptTxOut, TypedScriptTxOutRef)
import qualified Plutus.V1.Ledger.Value as Val
import Plutus.V2.Ledger.Api (TxId(..), TxOutRef(..))


-- | A transaction-output reference specific to Marlowe.
type MarloweTxOut = TypedScriptTxOut TypedMarloweValidator


-- | A transaction output specific to Marlowe.
type MarloweTxOutRef = TypedScriptTxOutRef TypedMarloweValidator


-- | History of a Marlowe contract.
data History =
    -- | The contract was created.
    Created
    {
      historyTxOutRef :: TxOutRef          -- ^ The UTxO that created the contract.
    , historyData     :: MarloweData       -- ^ The Marlowe data attached to the UTxO.
    , historyNext     :: Maybe History     -- ^ The next step in the history, if known.
    }
    -- | Input was applied to the contract.
  | InputApplied
    {
      historyInput    :: TransactionInput  -- ^ The Marlowe input that was applied.
    , historyTxOutRef :: TxOutRef          -- ^ The UTxO that resulted from the input being applied.
    , historyData     :: MarloweData       -- ^ The Marlowe data attached to the UTxO.
    , historyNext     :: Maybe History     -- ^ The next step in the history, if known.
    }
    -- | The contract was closed.
  | Closed
    {
      historyInput :: TransactionInput  -- ^ The Marlowe input that was applied.
    , historyTxId  :: TxId              -- ^ The transaction that resulted from the input being applied.
    }
    deriving stock (Eq, Generic, Show)
    -- FIXME:
    -- Ideally we want to drop all orphans but
    -- we are going to move this module to `marlowe-scripts`
    -- which can depend on `plutus-ledger` as a shortcut.
    -- deriving anyclass (ToJSON, FromJSON)


nextEntry :: History -> Maybe History
nextEntry Created { historyNext=n }     = n
nextEntry InputApplied { historyNext=n} = n
nextEntry Closed {}                     = Nothing

foldlHistory :: forall a. (a -> History -> a) -> a -> History -> a
foldlHistory f acc entry = case nextEntry entry of
  Just n  -> foldlHistory f (f acc entry) n
  Nothing -> f acc entry

foldrHistory :: forall a. (History -> a -> a) -> a -> History -> a
foldrHistory f zero entry = case nextEntry entry of
  Just n  -> f entry (foldrHistory f zero n)
  Nothing -> f entry zero


-- | A transaction-output reference specific to Marlowe role payout.
type RolePayoutTxOut = TypedScriptTxOut TypedRolePayoutValidator

-- | A transaction output specific to Marlowe role payout.
type RolePayoutTxOutRef = TypedScriptTxOutRef TypedRolePayoutValidator

data RolePayout =
    RolePayout
    {
      rolePayoutTxOutRef :: TxOutRef
    , rolePayoutName     :: Val.TokenName
    , rolePayoutValue    :: Val.Value
    }
    deriving stock (Eq, Generic, Show)
    -- deriving anyclass (ToJSON, FromJSON)

newtype IncludePkhTxns = IncludePkhTxns Bool

-- -- | Retrieve the history of a role-based Marlowe contract.
-- marloweHistory :: AsContractError e
--                => SlotConfig                      -- ^ The slot configuration.
--                -> MarloweParams                   -- ^ The Marlowe validator parameters.
--                -> IncludePkhTxns
--                -> Contract w s e (Maybe History)  -- ^ The original contract and the sequence of redemptions, if any.
-- marloweHistory slotConfig params (IncludePkhTxns includePkhTxns) =
--   do
--     let address = validatorAddress $ smallUntypedValidator params
--     logInfo $ "[DEBUG:marloweHistory] address = " <> show address
--     -- The script address contains transactions that have datum.
--     addressTxns <- txsAt address
--     logInfo $ "[DEBUG:marloweHistory] length addressTxns = " <> show (length addressTxns)
--     -- When a contract closes, there may be a UTxO at the role address.
--     roleTxns <- txsAt . scriptHashAddress $ rolePayoutValidatorHash params
--     logInfo $ "[DEBUG:marloweHistory] length roleTxns = " <> show (length roleTxns)
--
--     -- When a contract closes, there may be a UTxO at the owner's public key hash address.
--     -- We use this heavy query only on demand.
--     pkhTxns <- if includePkhTxns
--       then txsAt . flip Address Nothing . PubKeyCredential . unPaymentPubKeyHash =<< ownPaymentPubKeyHash
--       else pure []
--     logInfo $ "[DEBUG:marloweHistory] length pkhTxns = " <> show (length pkhTxns)
--     -- TODO: Extract all PKHs from the contract, and query these addresses, too.
--     pure
--       . history slotConfig params address
--       . nub
--       $ addressTxns <> roleTxns <> pkhTxns
--
-- marloweHistory' :: AsContractError e => SlotConfig -> MarloweParams -> Contract w s e (Maybe History)
-- marloweHistory' slotConfig params = marloweHistory slotConfig params (IncludePkhTxns False)
--
-- -- | Retrieve the history of a role-based Marlowe contract.
-- history :: SlotConfig      -- ^ The slot configuration.
--         -> MarloweParams   -- ^ The Marlowe validator parameters.
--         -> Address         -- ^ The Marlowe validator address.
--         -> [ChainIndexTx]  -- ^ The transactions at the Marlowe validator and role validator addresses.
--         -> Maybe History   -- ^ The original contract and the sequence of redemptions, if any.
-- history slotConfig params address citxs =
--   case histories slotConfig params address citxs of
--     -- If role tokens are minted by the "create" endpoint, then there should only ever be on contract at the address.
--     [history'] -> Just history'
--     -- Either there is no contract yet, or role tokens have been reused for multiple contracts.
--     _          -> Nothing
--
--
-- -- | Retrieve the histories of a role-based Marlowe contract.
-- histories :: SlotConfig      -- ^ The slot configuration.
--           -> MarloweParams   -- ^ The Marlowe validator parameters.
--           -> Address         -- ^ The Marlowe validator address.
--           -> [ChainIndexTx]  -- ^ The transactions at the Marlowe validator and role validator addresses.
--           -> [History]       -- ^ The original contracts and the sequence of redemptions.
-- histories slotConfig params address citxs =
--   [
--     Created (tyTxOutRefRef creation) (toMarloweState creation)
--       $ historyFrom slotConfig address citxs (tyTxOutRefRef creation)
--   |
--     creation <- creationTxOut params address `mapMaybe` citxs
--   ]
--
--
-- -- | Construct the sequence of redemptions following from a particular Marlowe transactions.
-- historyFrom :: SlotConfig          -- ^ The slot configuration.
--             -> Address             -- ^ The Marlowe validator address.
--             -> [ChainIndexTx]      -- ^ The transactions at the Marlowe validator and role validator addresses.
--             -> TxOutRef            -- ^ The Marlowe transaction to start from.
--             -> Maybe History       -- ^ The sequence of subsequent redemptions.
-- historyFrom slotConfig address citxs consumed =
--   let
--     anyInputsConsumed citx =
--       let
--         inputs = S.toList $ citx ^. citxInputs
--       in
--         any (\txIn -> consumed == txInRef txIn) inputs
--   in
--     -- The next redemption must consume the input.
--     case filter anyInputsConsumed citxs of
--       -- Only one transaction can consume the output of the previous step.
--       [citx] -> -- Find the redeemer
--                 case lookup consumed $ txInputs slotConfig citx of
--                   -- The output of the previous step was consumed with a redeemer.
--                   Just inputs -> -- Determine whether there is output to the script.
--                                  case filterOutputs address citx of
--                                    [mo] -> -- There was datum UTxO to the script, so the contract continues.
--                                            Just
--                                              . InputApplied inputs (tyTxOutRefRef mo) (toMarloweState mo)
--                                              $ historyFrom slotConfig address citxs (tyTxOutRefRef mo)
--                                    _    -> -- There was no datum UTxO to the script, so the contract is now closed.
--                                            Just
--                                              . Closed inputs
--                                              $ citx ^. citxTxId
--                   -- The output of the previous step couldn't have been consumed without a redeemer.
--                   _           -> Nothing
--       -- The output of the previous step hasn't been consumed, so the contract is still in progress.
--       _      -> Nothing
--
-- -- | Retrieve the states in UTxOs at the validator address.
-- marloweUtxoStatesAt :: AsContractError e
--                     => SmallTypedValidator                                                 -- ^ The Marlowe validator.
--                     -> Contract w s e ([MarloweTxOutRef], M.Map TxOutRef ChainIndexTxOut)  -- ^ Action for finding the Marlowe UTxOs at the validator.
-- marloweUtxoStatesAt validator =
--   do
--     utxos <- utxosTxOutTxAt $ validatorAddress validator
--     let
--       toMarlowe' (txOutRef, (citxOut, citx)) = toMarlowe citx (toTxOut citxOut) txOutRef
--     pure
--       (
--         mapMaybe toMarlowe' $ M.toList utxos
--       , fst <$> utxos
--       )
--
--
-- -- | Retrieve the Marlowe history from a transaction.
-- marloweHistoryFrom :: AsContractError e
--                    => SmallTypedValidator       -- ^ The Marlowe validator.
--                    -> SlotConfig                -- ^ The slot configuration.
--                    -> ChainIndexTx              -- ^ The transaction.
--                    -> Contract w s e [History]  -- ^ Action for finding the history for the transaction.
-- marloweHistoryFrom validator slotConfig citx =
--   do
--     let
--       valAddress = validatorAddress validator
--       toHistory (txOutRef, (citxOut, citx')) =
--         let
--           inputs = txInputs slotConfig citx'
--           toAddress = valAddress == (citxOut ^. ciTxOutAddress)
--           extractDatum (ScriptChainIndexTxOut _ _ (Right dat) _) = fromBuiltinData $ getDatum dat
--           extractDatum _                                         = Nothing
--           noDatum = null $ txMarloweData citx'
--         in
--           case (toAddress, inputs, extractDatum citxOut, noDatum) of
--             -- The address received the UTxO, no contract input was consumed, and datum is in the UTxO.
--             (True,  []          , Just md, _   ) -> Just $ Created txOutRef md Nothing
--             -- The address received the UTxO, contract input was consumed, and the datum is in the UTxO.
--             (True,  [(_, input)], Just md, _   ) -> Just $ InputApplied input txOutRef md Nothing
--             -- Another address received the UTxO, the contract input was consumed, but no datum is in the UTxO.
--             (False, [(_, input)], _      , True) -> Just $ Closed input $ citx' ^. citxTxId
--             -- An unexpected UTxO was found, violating the constraints on the script.
--             _                                    -> Nothing
--
--     utxos <- nub <$> utxosTxOutTxFromTx citx
--     logInfo $ "[DEBUG:marloweHistoryFrom] marloweStatesFrom | utxos | = " <> show (length utxos)
--     res <- for utxos \utxo -> do
--         let
--           hs = toHistory utxo
--         when (isNothing hs) $ do
--           logInfo "[DEBUG:marloweHistoryFrom] historyFrom encountered unexpected UTxO"
--         pure hs
--     pure $ nub . catMaybes $ res
--
--
-- -- | Retrieve the states in UTxOs of a transaction, optionally filtering them by the validator address.
-- marloweStatesFrom :: AsContractError e
--                   => Maybe SmallTypedValidator         -- ^ The Marlowe validator, if UTxOs are to be filtered by its address.
--                   -> ChainIndexTx                      -- ^ The transaction.
--                   -> Contract w s e [MarloweTxOutRef]  -- ^ Action for finding the UTxOs in the transaction, optionally filtered by the validator address.
-- marloweStatesFrom validator citx =
--   do
--     let
--       valAddress = validatorAddress <$> validator
--       addressFilter :: MarloweTxOutRef -> Maybe MarloweTxOutRef
--       addressFilter out =
--          do
--           guard
--             $ isNothing valAddress
--             || valAddress == Just (txOutAddress . tyTxOutTxOut $ tyTxOutRefOut out)
--           pure out
--       toMarlowe' (txOutRef, (citxOut, citx')) =
--         addressFilter
--           =<< toMarlowe citx' (toTxOut citxOut) txOutRef
--     mapMaybe toMarlowe'
--       <$> utxosTxOutTxFromTx citx
--
--
-- -- | Extract the Marlowe state from a Marlowe-specific output.
-- toMarloweState :: MarloweTxOutRef  -- ^ The Marlowe-specific output.
--                -> MarloweData      -- ^ The Marlowe data.
-- toMarloweState = tyTxOutData . tyTxOutRefOut
--
-- toRolePayout :: RolePayoutTxOutRef  -- ^ Role payout specific output
--              -> RolePayout          -- ^ The role payout.
-- toRolePayout t = RolePayout
--   (tyTxOutRefRef t)
--   (tyTxOutData . tyTxOutRefOut $ t)
--   (txOutValue . tyTxOutTxOut . tyTxOutRefOut $ t)
--
-- -- | Test whether a transaction created a Marlowe contract.
-- creationTxOut :: MarloweParams          -- ^ The Marlowe validator parameters.
--               -> Address                -- ^ The Marlowe validator address.
--               -> ChainIndexTx           -- ^ The transaction to be checked.
--               -> Maybe MarloweTxOutRef  -- ^ The creation-transaction output and the contract, if any.
-- creationTxOut MarloweParams{..} address citx =
--   do
--     -- Ensure that the transaction minted the role currency.
--     guard
--       . elem (ScriptHash $ unCurrencySymbol rolesCurrency)
--       . M.keys
--       $ citx ^. citxScripts
--     -- Find the output to the script address, if any.
--     case filterOutputs address citx of
--       [creation] -> Just creation
--       _          -> Nothing
--
--
-- -- | Find the Marlowe outputs at an address.
-- filterOutputs :: Address            -- ^ The Marlowe address.
--               -> ChainIndexTx       -- ^ The transaction.
--               -> [MarloweTxOutRef]  -- ^ The outputs to the Marlowe address.
-- filterOutputs address =
--   filter ((address ==) . txOutAddress . tyTxOutTxOut . tyTxOutRefOut)
--     . txMarloweData
--
--
-- -- | Extract Marlowe data from a transaction.
-- txMarloweData :: ChainIndexTx       -- ^ The transaction.
--               -> [MarloweTxOutRef]  -- ^ The outputs that have Marlowe data.
-- txMarloweData citx =
--   catMaybes
--     $ uncurry (toMarlowe citx)
--     <$> txDatums citx
--
-- txRoleData :: ChainIndexTx          -- ^ The transaction.
--            -> [RolePayoutTxOutRef]  -- ^ The outputs that have Marlowe data.
-- txRoleData citx =
--   catMaybes
--     $ uncurry (toTypedScriptTxOutRef citx)
--     <$> txDatums citx
--
-- -- | Make an output specific to Marlowe.
-- toMarlowe :: ChainIndexTx           -- ^ The transaction.
--           -> TxOut                  -- ^ The output.
--           -> TxOutRef               -- ^ The output reference.
--           -> Maybe MarloweTxOutRef  -- ^ The Marlowe-specific output, if any.
-- toMarlowe = toTypedScriptTxOutRef @TypedMarloweValidator
--
-- toRole :: ChainIndexTx
--         -> TxOut
--         -> TxOutRef
--         -> Maybe RolePayoutTxOutRef
-- toRole = toTypedScriptTxOutRef @TypedRolePayoutValidator
--
--
-- toTypedScriptTxOutRef :: forall a.
--                          (PlutusTx.IsData.Class.FromData (DatumType a),
--                           PlutusTx.IsData.Class.ToData (DatumType a))
--                       => ChainIndexTx                   -- ^ The transaction.
--                       -> TxOut                          -- ^ The output.
--                       -> TxOutRef                       -- ^ The output reference.
--                       -> Maybe (TypedScriptTxOutRef a)  -- ^ The Marlowe-specific output, if any.
-- toTypedScriptTxOutRef citx txOut txOutRef =
--   do
--     dh <- txOutDatumHash txOut
--     dat <- M.lookup dh $ citx ^. citxData
--     md <- fromBuiltinData $ getDatum dat
--     pure
--       . TypedScriptTxOutRef txOutRef
--       $ TypedScriptTxOut txOut md
--
--
-- -- | Extract transaction outputs with datum.
-- txDatums :: ChainIndexTx         -- ^ The transaction.
--          -> [(TxOut, TxOutRef)]  -- ^ The outputs that have datum.
-- txDatums citx =
--   case citx ^. citxOutputs of
--     ValidTx txOuts -> filter (\(txOut, _) -> isJust $ txOutDatumHash txOut)
--                       $ second (\i -> TxOutRef (citx ^. citxTxId) i)
--                       <$> zip txOuts [0..]
--     InvalidTx      -> []
--
--
-- -- | Extract Marlowe input from a transaction.
-- txInputs :: SlotConfig                      -- ^ The slot configuration.
--          -> ChainIndexTx                    -- ^ The transaction.
--          -> [(TxOutRef, TransactionInput)]  -- ^ The inputs that have Marlowe inputs.
-- txInputs slotConfig citx =
--   case slotRangeToPOSIXTimeRange slotConfig $ citx ^. citxValidRange of
--     Interval (LowerBound (Finite l) True) (UpperBound (Finite h) False) ->
--       let
--         slots = (l, h)
--       in
--         catMaybes
--           $ secondM (fmap (TransactionInput slots) . fromBuiltinData . getRedeemer)
--           <$> txRedeemers citx
--     _ -> []  -- TODO: Should this instead throw an error in an error monad?
--
--
-- -- | Extract transaction inputs with redeemers.
-- txRedeemers :: ChainIndexTx            -- ^ The transaction.
--             -> [(TxOutRef, Redeemer)]  -- ^ The inputs that have redeemers.
-- txRedeemers citx =
--   case citx ^. citxCardanoTx of
--     Just (SomeTx (ShelleyTx ShelleyBasedEraAlonzo tx@Alonzo.ValidatedTx{}) _) ->
--       catMaybes
--         [
--           do
--             (Alonzo.Data dat, _) <- Alonzo.indexedRdmrs tx $ Alonzo.Spending txin
--             pure (TxOutRef{..}, Redeemer . dataToBuiltinData $ dat)
--         |
--           txin@(Cardano.TxIn txid txix) <- S.toList . Alonzo.inputs . Alonzo.body $ tx
--         , let txOutRefId = TxId . toBuiltin . BS.drop 2 . toStrictByteString . toCBOR $ txid  -- TODO: Find a pre-existing function to convert Alonzo to Plutus TxIds.
--               txOutRefIdx = fromIntegral txix
--         ]
--     _ -> []
