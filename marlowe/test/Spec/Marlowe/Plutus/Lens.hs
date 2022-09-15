-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Missing lenses.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RankNTypes #-}


module Spec.Marlowe.Plutus.Lens
  ( -- * Operators
    (<>%~)
  , (<><~)
    -- * Lenses
  , scriptContextPurposeLens
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
  ) where


import Control.Lens (Lens', lens, use, (<>=))
import Control.Monad.State (MonadState)
import Plutus.V2.Ledger.Api
  ( DCert
  , Datum
  , DatumHash
  , Map
  , POSIXTimeRange
  , PubKeyHash
  , Redeemer
  , ScriptContext(scriptContextPurpose, scriptContextTxInfo)
  , ScriptPurpose
  , StakingCredential
  , TxId
  , TxInInfo
  , TxInfo(txInfoDCert, txInfoData, txInfoFee, txInfoId, txInfoInputs, txInfoMint, txInfoOutputs, txInfoRedeemers, txInfoReferenceInputs, txInfoSignatories, txInfoValidRange, txInfoWdrl)
  , TxOut
  , Value
  )


-- | Append a monadic value to a field.
(<><~) :: (MonadState s m, Semigroup a) => Lens' s a -> m a -> m ()
field <><~ x = (field <>=) =<< x


-- | Update a field with a monadic function.
(<>%~) :: (MonadState s m, Semigroup a) => Lens' s a -> (a -> m a) -> m ()
field <>%~ f =
  do
    value <- use field
    extra <- f value
    field <>= value <> extra


scriptContextTxInfoLens :: Lens' ScriptContext TxInfo
scriptContextTxInfoLens = lens scriptContextTxInfo $ \s x -> s {scriptContextTxInfo = x}


scriptContextPurposeLens :: Lens' ScriptContext ScriptPurpose
scriptContextPurposeLens = lens scriptContextPurpose $ \s x -> s {scriptContextPurpose = x}


txInfoInputsLens :: Lens' TxInfo [TxInInfo]
txInfoInputsLens = lens txInfoInputs $ \s x -> s {txInfoInputs = x}


txInfoReferenceInputsLens :: Lens' TxInfo [TxInInfo]
txInfoReferenceInputsLens = lens txInfoReferenceInputs $ \s x -> s {txInfoReferenceInputs = x}


txInfoOutputsLens :: Lens' TxInfo [TxOut]
txInfoOutputsLens = lens txInfoOutputs $ \s x -> s {txInfoOutputs = x}


txInfoFeeLens :: Lens' TxInfo Value
txInfoFeeLens = lens txInfoFee $ \s x ->  s {txInfoFee = x}


txInfoMintLens :: Lens' TxInfo Value
txInfoMintLens = lens txInfoMint $ \s x -> s {txInfoMint = x}


txInfoDCertLens :: Lens' TxInfo [DCert]
txInfoDCertLens = lens txInfoDCert $ \s x -> s {txInfoDCert = x}


txInfoWdrlLens :: Lens' TxInfo (Map StakingCredential Integer)
txInfoWdrlLens = lens txInfoWdrl $ \s x -> s {txInfoWdrl = x}


txInfoValidRangeLens :: Lens' TxInfo POSIXTimeRange
txInfoValidRangeLens = lens txInfoValidRange $ \s x -> s {txInfoValidRange = x}


txInfoSignatoriesLens :: Lens' TxInfo [PubKeyHash]
txInfoSignatoriesLens = lens txInfoSignatories $ \s x -> s {txInfoSignatories = x}


txInfoRedeemersLens :: Lens' TxInfo (Map ScriptPurpose Redeemer)
txInfoRedeemersLens = lens txInfoRedeemers $ \s x -> s {txInfoRedeemers = x}


txInfoDataLens :: Lens' TxInfo (Map DatumHash Datum)
txInfoDataLens = lens txInfoData $ \s x -> s {txInfoData = x}


txInfoIdLens :: Lens' TxInfo TxId
txInfoIdLens = lens txInfoId $ \s x -> s {txInfoId = x}
