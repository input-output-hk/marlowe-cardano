module Component.CurrencyInput.Types where

import Component.BigIntInput as BII
import Component.DecimalInput as DI
import Data.BigInt.Argonaut (BigInt)
import Data.Decimal (Decimal)
import Data.Maybe (Maybe)
import Data.Numbers.Natural (Natural)
import Data.Unit (Unit)
import Halogen as H

type Output = BigInt

data Action
  = AmountParseError String
  | ChangeValue BigInt
  | Receive Input

type DataRecord r =
  { amountInMinor :: BigInt
  -- ^ Initial value expressed in minor currency
  , classList :: Array String
  -- ^ Optional classes to style the component
  , currencySymbol :: Maybe String
  -- ^ Symbol that represents the major or just currency
  | r
  }

type Input = DataRecord
  ( majorCurrencyFactor :: Maybe Natural
  -- ^ A factor value between minor and major currency
  )

type State = DataRecord
  ( amountParseError :: Maybe String
  , majorCurrencyRatio ::
      Maybe
        { precision :: Natural
        -- ^ Precision used by internal decimal field to format plain String value
        , ratio :: Decimal
        -- ^ Precomputed ratio between minor and major currency
        }
  )

type ChildSlots =
  ( decimalInput :: forall q. H.Slot q DI.Output Unit
  , bigIntInput :: forall q. H.Slot q BII.Output Unit
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type Component query m = H.Component query Input Output m

type DSL m a =
  H.HalogenM State Action ChildSlots Output m a

data Query (a :: Type)

type Slot m = H.Slot Query Output m
