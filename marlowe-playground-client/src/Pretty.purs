module Pretty where

import Prologue
import Data.Array (concat, drop, dropWhile, length, replicate, take)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Map as Map
import Data.Maybe (maybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Halogen.HTML (HTML, abbr, text)
import Halogen.HTML.Properties (title)
import Marlowe.Extended.Metadata (NumberFormat(..), MetaData)
import Marlowe.Semantics (Party(..), Payee(..), Token(..))

renderPrettyToken :: forall p i. Token -> HTML p i
renderPrettyToken (Token "" "") = text "ADA"

renderPrettyToken (Token cur tok) = abbr [ title cur ] [ text tok ]

showPrettyToken :: Token -> String
showPrettyToken (Token "" "") = "ADA"

showPrettyToken (Token cur tok) = "\"" <> cur <> " " <> tok <> "\""

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

-- TODO I think this used to add commas? confirm this and add this back if
-- needed.
showPrettyMoney :: BigInt -> String
showPrettyMoney = BigInt.toString

renderPrettyPayee :: forall p i. MetaData -> Payee -> Array (HTML p i)
renderPrettyPayee metadata (Account owner2) =
  [ text "account of ", renderPrettyParty metadata owner2 ]

renderPrettyPayee metadata (Party dest) =
  [ text "party ", renderPrettyParty metadata dest ]

showBigIntAsCurrency :: BigInt -> Int -> String
showBigIntAsCurrency number numDecimals = fromCharArray numberStr
  where
  absValStr = replicate (numDecimals + 1) '0' <> toCharArray
    (BigInt.toString (if number < zero then -number else number))

  numDigits = length absValStr

  numDigitsBeforeSeparator = numDigits - numDecimals

  prefixStr = if number < zero then [ '-' ] else []

  digitsNoZeros = dropWhile (\x -> x == '0') $ take numDigitsBeforeSeparator
    absValStr

  digitsBeforeSeparator = if digitsNoZeros == [] then [ '0' ] else digitsNoZeros

  digitsAfterSeparator = take numDecimals $ drop numDigitsBeforeSeparator
    (concat [ absValStr, replicate numDecimals '0' ])

  numberStr = concat
    ( [ prefixStr, digitsBeforeSeparator ] <>
        if digitsAfterSeparator /= [] then [ [ '.' ], digitsAfterSeparator ]
        else []
    )

showPrettyChoice :: NumberFormat -> BigInt -> String
showPrettyChoice DefaultFormat num = showBigIntAsCurrency num 0

showPrettyChoice (DecimalFormat numDecimals strLabel) num = strLabel <> " " <>
  showBigIntAsCurrency num numDecimals

showPrettyChoice TimeFormat num = BigInt.toString num
