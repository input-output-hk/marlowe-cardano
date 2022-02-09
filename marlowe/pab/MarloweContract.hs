{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module MarloweContract(MarloweContract(..), handlers) where

import Control.Monad.Freer (interpret)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import qualified Language.Marlowe.Client as Marlowe
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), HasDefinitions (..),
                                            SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Prettyprinter (Pretty (..), viaShow)

data MarloweContract =
    MarloweApp -- The main marlowe contract
    | WalletCompanion -- Wallet companion contract
    | MarloweFollower -- Follower contract
    deriving (Eq, Ord, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

instance Pretty MarloweContract where
    pretty = viaShow

instance HasDefinitions MarloweContract where
    getDefinitions = [ MarloweApp
                     , WalletCompanion
                     , MarloweFollower
                     ]
    getSchema = const [] -- TODO: replace with proper schemas using Builtin.endpointsToSchemas (missing some instances currently)
    getContract = \case
        MarloweApp      -> SomeBuiltin Marlowe.marlowePlutusContract
        WalletCompanion -> SomeBuiltin Marlowe.marloweCompanionContract
        MarloweFollower -> SomeBuiltin Marlowe.marloweFollowContract

handlers :: SimulatorEffectHandlers (Builtin MarloweContract)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
