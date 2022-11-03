module Language.Marlowe.Runtime.SystemStart
  where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import Data.Set (Set)
import Data.Time (UTCTime)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Ouroboros
import Unsafe.Coerce (unsafeCoerce)

-- | A temporary workaround for the fact that Cardano.Api exposes APIs that
-- depend on SystemStart but don't export the type its self. This has be
-- rectified in recent versions, but still affects the current version. Remove
-- this module when we upgrade to a version of cardano-api that fixes this.
newtype SystemStart = SystemStart { unSystemStart :: UTCTime }
  deriving (Show)

makeTransactionBodyAutoBalance
  :: forall era mode
   . Cardano.IsShelleyBasedEra era
  => Cardano.EraInMode era mode
  -> SystemStart
  -> Cardano.EraHistory mode
  -> Cardano.ProtocolParameters
  -> Set Cardano.PoolId
  -> Cardano.UTxO era
  -> Cardano.TxBodyContent Cardano.BuildTx era
  -> Cardano.AddressInEra era -- ^ Change address
  -> Maybe Word       -- ^ Override key witnesses
  -> Either Cardano.TxBodyErrorAutoBalance (Cardano.BalancedTxBody era)
makeTransactionBodyAutoBalance eraInMode systemStart =
  Cardano.makeTransactionBodyAutoBalance eraInMode (unsafeCoerce systemStart)

querySystemStart
  :: forall mode
   . Cardano.LocalNodeConnectInfo mode
  -> IO (Either Ouroboros.AcquireFailure SystemStart)
querySystemStart connectInfo =
  fmap unsafeCoerce <$> Cardano.queryNodeLocalState connectInfo Nothing Cardano.QuerySystemStart
