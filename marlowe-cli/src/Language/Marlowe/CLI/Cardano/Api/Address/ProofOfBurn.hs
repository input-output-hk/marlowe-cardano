---------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Implementation of the proof-of-burn algorithm for Cardano.
-- | The `generateBurnAddress` function takes an arbitrary
-- | string as a seed (or tag) and creates a Cardano address
-- | where value is provably unspendable: i.e., no signing key
-- | exists that can witness spending eUTxOs at the address.
-- |
-- | Reference: Kostis Karantias, Aggelos Kiayias, Dionysis
-- | Zindros, "Proof of Burn", Financial Cryptography, 2020.
-- | <https://eprint.iacr.org/2019/1096.pdf>
--
---------------------------------------------------------------


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


module Language.Marlowe.CLI.Cardano.Api.Address.ProofOfBurn
  ( paymentCredential
  , permanentPublisher
  ) where


import Cardano.Api (PaymentCredential, ScriptHash)
import Cardano.Api.Shelley (fromShelleyPaymentCredential, toShelleyScriptHash)
import qualified Cardano.Crypto.Hash as C
import Cardano.Crypto.Hash.Class (hashFromBytes, hashToBytes, hashWith)
import Cardano.Ledger.Credential (Credential(KeyHashObj))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyHash(KeyHash), KeyRole(Payment))
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import Data.Bits (xor)
import Data.ByteString (ByteString, pack, unpack)


-- | Generate a reference script unique unspendable publisher address.
permanentPublisher :: ScriptHash
                   -> PaymentCredential
permanentPublisher h = do
  let
    (Shelley.ScriptHash h') = toShelleyScriptHash h
  paymentCredential $ hashToBytes h'


-- | Generate a proof-of-burn address.
paymentCredential :: ByteString         -- ^ Any bytestring.
                  -> PaymentCredential
paymentCredential tag = do
  let
    KeyHash h = hashTag tag
    h' = tweakHash h
  fromShelleyPaymentCredential $ KeyHashObj $ KeyHash h'


-- | Convert a byte string into a payment key hash.
hashTag :: ByteString                      -- ^ Any bytestring.
        -> KeyHash 'Payment StandardCrypto -- ^ The hash.
hashTag tag = KeyHash $ hashWith (const tag) undefined


-- | Flip the last bit of a payment key hash.
tweakHash :: C.HashAlgorithm h1 => C.Hash h2 a1 -> C.Hash h1 a2
tweakHash hashed = do
  let
    original = unpack $ hashToBytes hashed
    tweaked = init original ++ [last original `xor` 1]
    Just tweaked' = hashFromBytes $ pack tweaked
  tweaked'
