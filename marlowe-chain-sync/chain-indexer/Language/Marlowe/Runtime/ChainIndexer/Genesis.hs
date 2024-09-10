module Language.Marlowe.Runtime.ChainIndexer.Genesis (
  GenesisBlock (..),
  GenesisTx (..),
  computeGenesisBlock,
) where

import Cardano.Api (AddressAny (..), AsType (..), BlockHeader, Hash, TxId, deserialiseFromRawBytes)
import Cardano.Api.Byron (Address (..))
import Cardano.Api.Ledger (Coin)
import Cardano.Api.Shelley (Hash (..), ShelleyGenesis (..), fromShelleyAddrToAny, fromShelleyTxId)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, serializeCborHash)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as Shelley
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString.Short (toShort)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data GenesisBlock = GenesisBlock
  { genesisBlockHash :: !(Hash BlockHeader)
  , genesisBlockTxs :: !(Set GenesisTx)
  }
  deriving (Eq, Show)

data GenesisTx = GenesisTx
  { genesisTxId :: !TxId
  , genesisTxLovelace :: !Coin
  , genesisTxAddress :: !AddressAny
  }
  deriving (Eq, Ord, Show)

computeGenesisBlock :: ByteString -> Byron.Config -> ShelleyGenesis StandardCrypto -> GenesisBlock
computeGenesisBlock genesisFileHash byronGenesis shelleyGenesis =
  GenesisBlock
    { genesisBlockHash = HeaderHash $ toShort genesisFileHash
    , genesisBlockTxs =
        Set.fromList $
          mconcat
            [ uncurry fromByronBalance <$> byronGenesisUTxOs byronGenesis
            , uncurry fromShelleyBalance <$> shelleyGenesisUTxOs shelleyGenesis
            ]
    }

byronGenesisUTxOs :: Byron.Config -> [(Byron.Address, Byron.Lovelace)]
byronGenesisUTxOs config = avvmBalances <> nonAvvmBalances
  where
    avvmBalances :: [(Byron.Address, Byron.Lovelace)]
    avvmBalances =
      first (Byron.makeRedeemAddress networkMagic . Crypto.fromCompactRedeemVerificationKey)
        <$> Map.toList (Byron.unGenesisAvvmBalances $ Byron.configAvvmDistr config)

    networkMagic :: Byron.NetworkMagic
    networkMagic = Byron.makeNetworkMagic (Byron.configProtocolMagic config)

    nonAvvmBalances :: [(Byron.Address, Byron.Lovelace)]
    nonAvvmBalances = Map.toList (Byron.unGenesisNonAvvmBalances $ Byron.configNonAvvmBalances config)

fromByronBalance :: Byron.Address -> Byron.Lovelace -> GenesisTx
fromByronBalance address lovelace =
  GenesisTx
    { genesisTxId =
        either (error . (<> "fromByronBalance: ") . show) id $
          deserialiseFromRawBytes AsTxId $
            abstractHashToBytes $
              serializeCborHash address
    , genesisTxLovelace = fromIntegral $ Byron.lovelaceToInteger lovelace
    , genesisTxAddress = AddressByron $ ByronAddress address
    }

shelleyGenesisUTxOs
  :: ShelleyGenesis StandardCrypto
  -> [(Shelley.TxIn StandardCrypto, Core.TxOut (ShelleyEra StandardCrypto))]
shelleyGenesisUTxOs = Map.toList . Shelley.unUTxO . Shelley.genesisUTxO

fromShelleyBalance :: Shelley.TxIn StandardCrypto -> Core.TxOut (ShelleyEra StandardCrypto) -> GenesisTx
fromShelleyBalance (Shelley.TxIn txId _) (Shelley.ShelleyTxOut addr coin) =
  GenesisTx
    { genesisTxId = fromShelleyTxId txId
    , genesisTxLovelace = coin
    , genesisTxAddress = fromShelleyAddrToAny addr
    }
