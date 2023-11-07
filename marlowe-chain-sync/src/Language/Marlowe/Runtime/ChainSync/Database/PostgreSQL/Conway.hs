{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Conway where

import Cardano.Ledger.Allegra.TxBody (StrictMaybe (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), txdats')
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.Babbage (BabbageEra, BabbageTxOut)
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized (..), shelleyProtVer)
import qualified Cardano.Ledger.Binary as L
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Crypto
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxInRows, alonzoTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxOutRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types
import Unsafe.Coerce (unsafeCoerce)

conwayTxToRows :: Int64 -> Bytea -> Bytea -> AlonzoTx (ConwayEra StandardCrypto) -> TxRowGroup
conwayTxToRows slotNo blockHash txId tx@AlonzoTx{..} =
  ( alonzoTxRow encodeConwayMetadata slotNo blockHash txId (ctbVldt body) auxiliaryData isValid
  , alonzoTxInRows slotNo txId isValid tx (ctbSpendInputs body) (ctbCollateralInputs body)
  , babbageTxOutRows
      slotNo
      txId
      isValid
      (coerceDats $ txdats' wits)
      (coerceTxOut <$> ctbCollateralReturn body)
      (coerceTxOut <$> toList (ctbOutputs body))
  , maryAssetMintRows slotNo txId $ ctbMint body
  )

encodeConwayMetadata :: AlonzoTxAuxData (ConwayEra StandardCrypto) -> ByteString
encodeConwayMetadata (AlonzoTxAuxData md _ _) = L.serialize' shelleyProtVer md

coerceTxOut
  :: Sized (BabbageTxOut (ConwayEra StandardCrypto))
  -> Sized (BabbageTxOut (BabbageEra StandardCrypto))
coerceTxOut (Sized (BabbageTxOut addr value datum _) size) =
  Sized (BabbageTxOut addr value (unsafeCoerce datum) SNothing) size

coerceDats :: TxDats (ConwayEra StandardCrypto) -> TxDats (BabbageEra StandardCrypto)
coerceDats = unsafeCoerce
