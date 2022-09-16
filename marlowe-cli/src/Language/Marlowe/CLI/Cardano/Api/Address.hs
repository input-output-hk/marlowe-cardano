module Language.Marlowe.CLI.Cardano.Api.Address
  where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Shelley.API as CS

toShelleyStakeReference :: C.StakeAddressReference
                        -> CS.StakeReference CC.StandardCrypto
toShelleyStakeReference (C.StakeAddressByValue stakecred) =
    CS.StakeRefBase (CS.toShelleyStakeCredential stakecred)
toShelleyStakeReference (C.StakeAddressByPointer ptr) =
    CS.StakeRefPtr (C.unStakeAddressPointer ptr)
toShelleyStakeReference  C.NoStakeAddress =
    CS.StakeRefNull
