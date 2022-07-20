module Pretty where

import Prologue

import Contrib.Data.Decimal (fromBigInt) as D
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BI
import Data.BigInt.Argonaut as BigInt
import Data.Decimal (fromInt, pow, toFixed) as D
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Numbers.Natural (Natural(..))
import Data.Numbers.Natural as N
import Data.String as String
import Halogen.HTML (HTML, abbr, text)
import Halogen.HTML.Properties (title)
import Language.Marlowe.Core.V1.Semantics.Types (Party(..), Payee(..))
import Language.Marlowe.Extended.V1.Metadata (MetaData, NumberFormat(..))

renderPrettyParty :: forall p i. MetaData -> Party -> HTML p i
renderPrettyParty _ (PK pkh) =
  if String.length pkh > 10 then abbr [ title $ "pubkey " <> pkh ]
    [ text $ String.take 10 pkh ]
  else text pkh

renderPrettyParty metadata (Role role) = abbr
  [ title $ "role " <> role <> explanationOrEmptyString ]
  [ text role ]
  where
  explanationOrEmptyString =
    maybe "" (\explanation -> " – “" <> explanation <> "„")
      $ Map.lookup role metadata.roleDescriptions

showPrettyParty :: Party -> String
showPrettyParty (PK pkh) = "PubKey " <> pkh

showPrettyParty (Role role) = show role

renderPrettyPayee :: forall p i. MetaData -> Payee -> Array (HTML p i)
renderPrettyPayee metadata (Account owner2) =
  [ text "account of ", renderPrettyParty metadata owner2 ]

renderPrettyPayee metadata (Party dest) =
  [ renderPrettyParty metadata dest, text " wallet" ]

showPrettyChoice :: NumberFormat -> BigInt -> String
showPrettyChoice DefaultFormat num = BI.toString num

showPrettyChoice (DecimalFormat numDecimals strLabel) num =
  strLabel <> " " <> case N.fromInt numDecimals of
    Just (Natural n) ->
      D.toFixed n (D.fromBigInt num / (D.fromInt 10 `D.pow` D.fromInt n))
    Nothing ->
      BI.toString num

showPrettyChoice TimeFormat num = BigInt.toString num
