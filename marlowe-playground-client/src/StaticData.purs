module StaticData
  ( haskellBufferLocalStorageKey
  , jsBufferLocalStorageKey
  , demoFiles
  , demoFilesJS
  , demoFilesMetadata
  , marloweBufferLocalStorageKey
  , simulatorBufferLocalStorageKey
  , marloweContract
  , marloweContracts
  , gistIdLocalStorageKey
  , sessionStorageKey
  ) where

import Data.Lens (view)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Examples.Haskell.Contracts
  ( contractForDifferences
  , contractForDifferencesWithOracle
  , couponBondGuaranteed
  , escrow
  , escrowWithCollateral
  , example
  , swap
  , zeroCouponBond
  ) as HE
import Examples.JS.Contracts
  ( contractForDifferences
  , contractForDifferencesWithOracle
  , couponBondGuaranteed
  , escrow
  , escrowWithCollateral
  , example
  , swap
  , zeroCouponBond
  ) as JSE
import Examples.Marlowe.Contracts
  ( contractForDifferences
  , contractForDifferencesWithOracle
  , couponBondGuaranteed
  , escrow
  , escrowWithCollateral
  , example
  , swap
  , zeroCouponBond
  ) as ME
import Examples.PureScript.Modules
  ( contractForDifferences
  , contractForDifferencesWithOracle
  , couponBondGuaranteed
  , escrow
  , escrowWithCollateral
  , swap
  , zeroCouponBond
  ) as M
import Language.Marlowe.Extended.V1 (_metadata)
import Language.Marlowe.Extended.V1.Metadata (emptyContractMetadata)
import Language.Marlowe.Extended.V1.Metadata.Types (MetaData)
import LocalStorage as LocalStorage

type Label = String

type Contents = String

demoFiles
  :: Map Label Contents
demoFiles =
  Map.fromFoldable
    [ "Example" /\ HE.example
    , "Escrow" /\ HE.escrow
    , "EscrowWithCollateral" /\ HE.escrowWithCollateral
    , "ZeroCouponBond" /\ HE.zeroCouponBond
    , "CouponBondGuaranteed" /\ HE.couponBondGuaranteed
    , "Swap" /\ HE.swap
    , "CFD" /\ HE.contractForDifferences
    , "CFDWithOracle" /\ HE.contractForDifferencesWithOracle
    ]

demoFilesJS
  :: Map Label Contents
demoFilesJS =
  Map.fromFoldable
    [ "Example" /\ JSE.example
    , "Escrow" /\ JSE.escrow
    , "EscrowWithCollateral" /\ JSE.escrowWithCollateral
    , "ZeroCouponBond" /\ JSE.zeroCouponBond
    , "CouponBondGuaranteed" /\ JSE.couponBondGuaranteed
    , "Swap" /\ JSE.swap
    , "CFD" /\ JSE.contractForDifferences
    , "CFDWithOracle" /\ JSE.contractForDifferencesWithOracle
    ]

marloweContracts
  :: Map Label Contents
marloweContracts =
  Map.fromFoldable
    [ "Example" /\ ME.example
    , "Escrow" /\ ME.escrow
    , "EscrowWithCollateral" /\ ME.escrowWithCollateral
    , "ZeroCouponBond" /\ ME.zeroCouponBond
    , "CouponBondGuaranteed" /\ ME.couponBondGuaranteed
    , "Swap" /\ ME.swap
    , "CFD" /\ ME.contractForDifferences
    , "CFDWithOracle" /\ ME.contractForDifferencesWithOracle
    ]

demoFilesMetadata
  :: Map Label MetaData
demoFilesMetadata =
  Map.fromFoldable
    [ "Example" /\ emptyContractMetadata
    , "Escrow" /\ view _metadata M.escrow
    , "EscrowWithCollateral" /\ view _metadata M.escrowWithCollateral
    , "ZeroCouponBond" /\ view _metadata M.zeroCouponBond
    , "CouponBondGuaranteed" /\ view _metadata M.couponBondGuaranteed
    , "Swap" /\ view _metadata M.swap
    , "CFD" /\ view _metadata M.contractForDifferences
    , "CFDWithOracle" /\ view _metadata M.contractForDifferencesWithOracle
    ]

marloweContract
  :: Contents
marloweContract = "(Some Marlowe Code)"

haskellBufferLocalStorageKey
  :: LocalStorage.Key
haskellBufferLocalStorageKey = LocalStorage.Key "HaskellBuffer"

jsBufferLocalStorageKey
  :: LocalStorage.Key
jsBufferLocalStorageKey = LocalStorage.Key "JavascriptBuffer"

marloweBufferLocalStorageKey
  :: LocalStorage.Key
marloweBufferLocalStorageKey = LocalStorage.Key "MarloweBuffer"

simulatorBufferLocalStorageKey
  :: LocalStorage.Key
simulatorBufferLocalStorageKey = LocalStorage.Key "SimulationBuffer"

gistIdLocalStorageKey
  :: LocalStorage.Key
gistIdLocalStorageKey = LocalStorage.Key "GistId"

sessionStorageKey :: LocalStorage.Key
sessionStorageKey = LocalStorage.Key "MarlowePlaygroundSession"
