module Language.Marlowe.CLI.Cardano.Api.Address where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Crypto qualified as CC
import Cardano.Ledger.Shelley.API qualified as CS

toShelleyStakeReference
  :: C.StakeAddressReference
  -> CS.StakeReference CC.StandardCrypto
toShelleyStakeReference (C.StakeAddressByValue stakecred) =
  CS.StakeRefBase (CS.toShelleyStakeCredential stakecred)
toShelleyStakeReference (C.StakeAddressByPointer ptr) =
  CS.StakeRefPtr (C.unStakeAddressPointer ptr)
toShelleyStakeReference C.NoStakeAddress =
  CS.StakeRefNull
