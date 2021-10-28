module Marlowe.PAB
  ( transactionFee
  , contractCreationFee
  , PlutusAppId(..)
  ) where

import Prologue

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UUID.Argonaut (UUID)

-- In the Marlowe PAB, transactions have a fixed cost of 10 lovelace; in the real node, transaction
-- fees will vary, but this will serve as an approximation for now.
transactionFee :: BigInt
transactionFee = fromInt 10

contractCreationFee :: BigInt
contractCreationFee = transactionFee

{-
A `PlutusAppId` is used to identify an instance of a Plutus "contract" in the PAB. In the PAB code it is
called `ContractInstanceId` - as above, we don't refer to "contracts" here so as to avoid confusion with
*Marlowe* contracts. This is converted to a `ContractInstanceId` that the PAB understands by the `Bridge`
module.
-}
newtype PlutusAppId
  = PlutusAppId UUID

derive instance newtypePlutusAppId :: Newtype PlutusAppId _

derive instance eqPlutusAppId :: Eq PlutusAppId

derive instance ordPlutusAppId :: Ord PlutusAppId

derive instance genericPlutusAppId :: Generic PlutusAppId _

-- note we need to encode this type, not to communicate with the PAB (we have the `ContractInstanceId`
-- for that), but to save `WalletData` to local storage
derive newtype instance encodeJsonPlutusAppId :: EncodeJson PlutusAppId

derive newtype instance decodeJsonPlutusAppId :: DecodeJson PlutusAppId
