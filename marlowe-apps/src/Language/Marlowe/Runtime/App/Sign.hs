

module Language.Marlowe.Runtime.App.Sign
  ( sign
  ) where


import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)

import qualified Cardano.Api as C
  ( IsShelleyBasedEra
  , PaymentExtendedKey
  , PaymentKey
  , ShelleyWitnessSigningKey(WitnessPaymentExtendedKey, WitnessPaymentKey)
  , SigningKey
  , Tx
  , TxBody
  , getTxId
  , signShelleyTransaction
  )


sign :: C.IsShelleyBasedEra era
     => C.TxBody era
     -> [C.SigningKey C.PaymentKey]
     -> [C.SigningKey C.PaymentExtendedKey]
     -> (TxId, C.Tx era)
sign body paymentKeys paymentExtendedKeys =
  let
    tx =
      C.signShelleyTransaction body
        $ fmap C.WitnessPaymentKey paymentKeys
        <> fmap C.WitnessPaymentExtendedKey paymentExtendedKeys
  in
    (fromCardanoTxId $ C.getTxId body, tx)
