-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for tests of Marlowe's Plutus implementation.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Plutus.Types
  ( -- *Types
    PayoutTransaction(..)
  , PlutusTransaction(..)
  , SemanticsTransaction(..)
    -- * Lenses
  , amount
  , amountLens
  , datum
  , infoDCert
  , infoData
  , infoFee
  , infoId
  , infoInputs
  , infoMint
  , infoOutputs
  , infoRedeemers
  , infoReferenceInputs
  , infoSignatories
  , infoValidRange
  , infoWdrl
  , input
  , inputContract
  , inputContractLens
  , inputLens
  , inputState
  , inputStateLens
  , marloweParams
  , marloweParamsPayout
  , output
  , outputLens
  , parameters
  , paramsLens
  , paramsLens'
  , redeemer
  , role
  , roleLens
  , scriptContext
  , scriptPurpose
  , txInfo
  ) where


import Control.Lens (Lens', lens)
import Language.Marlowe.Core.V1.Semantics (MarloweParams, TransactionInput, TransactionOutput)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State)
import Plutus.V2.Ledger.Api
  ( DCert
  , Datum
  , DatumHash
  , Map
  , POSIXTimeRange
  , PubKeyHash
  , Redeemer
  , ScriptContext
  , ScriptPurpose
  , StakingCredential
  , TokenName
  , TxId
  , TxInInfo
  , TxInfo
  , TxOut
  , Value
  )
import Spec.Marlowe.Plutus.Lens
  ( scriptContextPurposeLens
  , scriptContextTxInfoLens
  , txInfoDCertLens
  , txInfoDataLens
  , txInfoFeeLens
  , txInfoIdLens
  , txInfoInputsLens
  , txInfoMintLens
  , txInfoOutputsLens
  , txInfoRedeemersLens
  , txInfoReferenceInputsLens
  , txInfoSignatoriesLens
  , txInfoValidRangeLens
  , txInfoWdrlLens
  )


-- | A Plutus transaction.
data PlutusTransaction a =
  PlutusTransaction
  {
    _parameters    :: a              -- ^ Parameters providing context.
  , _datum         :: Datum          -- ^ The datum.
  , _redeemer      :: Redeemer       -- ^ The redeemer.
  , _scriptContext :: ScriptContext  -- ^ The script context.
  }
    deriving Show


parameters :: Lens' (PlutusTransaction a) a
parameters = lens _parameters $ \s x -> s {_parameters = x}


datum :: Lens' (PlutusTransaction a) Datum
datum = lens _datum $ \s x -> s {_datum = x}


redeemer :: Lens' (PlutusTransaction a) Redeemer
redeemer = lens _redeemer $ \s x -> s {_redeemer = x}


scriptContext :: Lens' (PlutusTransaction a) ScriptContext
scriptContext = lens _scriptContext $ \s x -> s {_scriptContext = x}


txInfo :: Lens' (PlutusTransaction a) TxInfo
txInfo = scriptContext . scriptContextTxInfoLens


scriptPurpose :: Lens' (PlutusTransaction a) ScriptPurpose
scriptPurpose = scriptContext .scriptContextPurposeLens


infoInputs :: Lens' (PlutusTransaction a) [TxInInfo]
infoInputs = scriptContext . scriptContextTxInfoLens . txInfoInputsLens


infoReferenceInputs :: Lens' (PlutusTransaction a) [TxInInfo]
infoReferenceInputs = scriptContext . scriptContextTxInfoLens . txInfoReferenceInputsLens


infoOutputs :: Lens' (PlutusTransaction a) [TxOut]
infoOutputs = scriptContext . scriptContextTxInfoLens . txInfoOutputsLens


infoFee :: Lens' (PlutusTransaction a) Value
infoFee = scriptContext . scriptContextTxInfoLens . txInfoFeeLens


infoMint :: Lens' (PlutusTransaction a) Value
infoMint = scriptContext . scriptContextTxInfoLens . txInfoMintLens


infoDCert :: Lens' (PlutusTransaction a) [DCert]
infoDCert = scriptContext . scriptContextTxInfoLens . txInfoDCertLens


infoWdrl :: Lens' (PlutusTransaction a) (Map StakingCredential Integer)
infoWdrl = scriptContext . scriptContextTxInfoLens . txInfoWdrlLens


infoValidRange :: Lens' (PlutusTransaction a) POSIXTimeRange
infoValidRange = scriptContext . scriptContextTxInfoLens . txInfoValidRangeLens


infoSignatories :: Lens' (PlutusTransaction a) [PubKeyHash]
infoSignatories = scriptContext . scriptContextTxInfoLens . txInfoSignatoriesLens


infoRedeemers :: Lens' (PlutusTransaction a) (Map ScriptPurpose Redeemer)
infoRedeemers = scriptContext . scriptContextTxInfoLens . txInfoRedeemersLens


infoData :: Lens' (PlutusTransaction a) (Map DatumHash Datum)
infoData = scriptContext . scriptContextTxInfoLens . txInfoDataLens


infoId :: Lens' (PlutusTransaction a) TxId
infoId = scriptContext . scriptContextTxInfoLens . txInfoIdLens


-- | A Marlowe semantics transaction.
data SemanticsTransaction =
  SemanticsTransaction
  {
    _params   :: MarloweParams      -- ^ The parameters.
  , _state    :: State              -- ^ The incoming state.
  , _contract :: Contract           -- ^ The incoming contract.
  , _input    :: TransactionInput   -- ^ The transaction input.
  , _output   :: TransactionOutput  -- ^ The transaction output.
  }
    deriving Show


paramsLens :: Lens' SemanticsTransaction MarloweParams
paramsLens = lens _params $ \s x -> s {_params = x}


marloweParams :: Lens' (PlutusTransaction SemanticsTransaction) MarloweParams
marloweParams = parameters . paramsLens


inputStateLens :: Lens' SemanticsTransaction State
inputStateLens = lens _state $ \s x -> s {_state = x}


inputState :: Lens' (PlutusTransaction SemanticsTransaction) State
inputState = parameters . inputStateLens


inputContractLens :: Lens' SemanticsTransaction Contract
inputContractLens = lens _contract $ \s x -> s {_contract = x}


inputContract :: Lens' (PlutusTransaction SemanticsTransaction) Contract
inputContract = parameters . inputContractLens


inputLens :: Lens' SemanticsTransaction TransactionInput
inputLens = lens _input $ \s x -> s {_input = x}


input :: Lens' (PlutusTransaction SemanticsTransaction) TransactionInput
input = parameters . inputLens


outputLens :: Lens' SemanticsTransaction TransactionOutput
outputLens = lens _output $ \s x -> s {_output = x}


output :: Lens' (PlutusTransaction SemanticsTransaction) TransactionOutput
output = parameters . outputLens


-- | A Marlowe payout transaction.
data PayoutTransaction =
  PayoutTransaction
  {
    _params' :: MarloweParams  -- ^ The parameters.
  , _role    :: TokenName      -- ^ The role name.
  , _amount  :: Value          -- ^ The value paid.
  }
    deriving Show


paramsLens' :: Lens' PayoutTransaction MarloweParams
paramsLens' = lens _params' $ \s x -> s {_params' = x}


marloweParamsPayout :: Lens' (PlutusTransaction PayoutTransaction) MarloweParams
marloweParamsPayout = parameters . paramsLens'


roleLens :: Lens' PayoutTransaction TokenName
roleLens = lens _role $ \s x -> s {_role = x}


role :: Lens' (PlutusTransaction PayoutTransaction) TokenName
role = parameters . roleLens


amountLens :: Lens' PayoutTransaction Value
amountLens = lens _amount $ \s x -> s {_amount = x}


amount :: Lens' (PlutusTransaction PayoutTransaction) Value
amount = parameters . amountLens
