module Language.Marlowe.Runtime.ChainSync.Genesis
  ( GenesisBlock (..)
  , GenesisTx (..)
  , computeByronGenesisBlock
  ) where

import Cardano.Api (AddressAny(..), AsType(..), BlockHeader, Hash, Lovelace, TxId, deserialiseFromRawBytes)
import Cardano.Api.Byron (Address(..))
import Cardano.Api.Shelley (Hash(..))
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, serializeCborHash)
import qualified Cardano.Crypto as Crypto
import Data.Bifunctor (Bifunctor(..))
import Data.ByteString (ByteString)
import Data.ByteString.Short (toShort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data GenesisBlock = GenesisBlock
  { genesisBlockHash :: !(Hash BlockHeader)
  , genesisBlockTxs  :: !(Set GenesisTx)
  } deriving (Eq)

data GenesisTx = GenesisTx
  { genesisTxId       :: !TxId
  , genesisTxLovelace :: !Lovelace
  , genesisTxAddress  :: !AddressAny
  } deriving (Eq, Ord)

computeByronGenesisBlock :: ByteString -> Byron.Config -> GenesisBlock
computeByronGenesisBlock genesisFileHash genesisConfig = GenesisBlock
  { genesisBlockHash = HeaderHash $ toShort genesisFileHash
  , genesisBlockTxs = Set.fromList $ fromByronBalance <$> genesisTxos genesisConfig
  }

genesisTxos :: Byron.Config -> [(Byron.Address, Byron.Lovelace)]
genesisTxos config = avvmBalances <> nonAvvmBalances
  where
    avvmBalances :: [(Byron.Address, Byron.Lovelace)]
    avvmBalances =
      first (Byron.makeRedeemAddress networkMagic . Crypto.fromCompactRedeemVerificationKey)
        <$> Map.toList (Byron.unGenesisAvvmBalances $ Byron.configAvvmDistr config)

    networkMagic :: Byron.NetworkMagic
    networkMagic = Byron.makeNetworkMagic (Byron.configProtocolMagic config)

    nonAvvmBalances :: [(Byron.Address, Byron.Lovelace)]
    nonAvvmBalances = Map.toList (Byron.unGenesisNonAvvmBalances $ Byron.configNonAvvmBalances config)

fromByronBalance :: (Byron.Address, Byron.Lovelace) -> GenesisTx
fromByronBalance (address, lovelace) = GenesisTx
  { genesisTxId = fromMaybe (error "fromByronBalance")
      $ deserialiseFromRawBytes AsTxId
      $ abstractHashToBytes
      $ serializeCborHash address
  , genesisTxLovelace = fromIntegral $ Byron.lovelaceToInteger lovelace
  , genesisTxAddress = AddressByron $ ByronAddress address
  }
