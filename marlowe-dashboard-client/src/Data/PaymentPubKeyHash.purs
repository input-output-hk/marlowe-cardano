module Data.PaymentPubKeyHash
  ( PaymentPubKeyHash
  , _PaymentPubKeyHash
  , fromPubKeyHash
  , toPubKeyHash
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  , (.:)
  )
import Data.Lens (Iso', iso)
import Data.PubKeyHash (PubKeyHash)
import Data.ShowUtils (parens)

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Eq PaymentPubKeyHash
derive instance Ord PaymentPubKeyHash
instance Show PaymentPubKeyHash where
  show = parens <<< append "fromPubKeyHash " <<< show <<< toPubKeyHash

instance EncodeJson PaymentPubKeyHash where
  encodeJson (PaymentPubKeyHash unPaymentPubKeyHash) =
    encodeJson { unPaymentPubKeyHash }

instance DecodeJson PaymentPubKeyHash where
  decodeJson json = do
    obj <- decodeJson json
    phk <- obj .: "unPaymentPubKeyHash"
    pure $ PaymentPubKeyHash phk

fromPubKeyHash :: PubKeyHash -> PaymentPubKeyHash
fromPubKeyHash = PaymentPubKeyHash

toPubKeyHash :: PaymentPubKeyHash -> PubKeyHash
toPubKeyHash (PaymentPubKeyHash pkh) = pkh

_PaymentPubKeyHash :: Iso' PaymentPubKeyHash PubKeyHash
_PaymentPubKeyHash = iso toPubKeyHash fromPubKeyHash
