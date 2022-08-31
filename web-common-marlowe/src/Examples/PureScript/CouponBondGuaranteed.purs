module Examples.PureScript.CouponBondGuaranteed
  ( contractModule
  , metadata
  , contract
  ) where

import Prelude

import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types (Party(..), Token(..))
import Language.Marlowe.Extended.V1
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Module(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Language.Marlowe.Extended.V1.Metadata (lovelaceFormat)
import Language.Marlowe.Extended.V1.Metadata.Types (ContractType(..), MetaData)
import Marlowe.Time (unsafeInstantFromInt)
import Plutus.V1.Ledger.Time (POSIXTime(..))

contractModule :: Module
contractModule = Module { metadata, contract }

metadata :: MetaData
metadata =
  { contractType: CouponBondGuaranteed
  , contractName: "Coupon Bond Guaranteed"
  , contractShortDescription:
      "Debt agreement between an _**Lender**_ and an _**Borrower**_ that must be repaid in 3 instalments."
  , contractLongDescription:
      "_**Lender**_ will advance the _**Principal**_ amount at the beginning of the contract, and the _**Borrower**_ will pay back _**Interest instalment**_ every 30 slots and the _**Principal**_ amount by the end of 3 instalments. The debt is backed by a collateral provided by the _**Guarantor**_ which will be refunded as long as the _**Borrower**_ pays back on time."
  , choiceInfo: Map.empty
  , roleDescriptions:
      ( Map.fromFoldable
          [ "Guarantor" /\
              "Provides a collateral in case the _**Borrower**_ defaults."
          , "Lender" /\ "Provides the money that the _**Borrower**_ borrows."
          , "Borrower" /\
              "Borrows the money provided by the _**Lender**_ and returns it together with three _**Interest instalment**_s."
          ]
      )
  , timeParameterDescriptions: mempty
  , valueParameterInfo:
      ( OMap.fromFoldable
          [ "Interest instalment"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription:
                    "Amount of Lovelace that will be paid by the _**Borrower**_ every 30 slots for 3 iterations."
                }
          , "Principal"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription:
                    "Amount of Lovelace that will be borrowed by the _**Borrower**_."
                }
          ]
      )
  }

ada :: Token
ada = Token "" ""

guarantor :: Party
guarantor = Role "Guarantor"

investor :: Party
investor = Role "Lender"

issuer :: Party
issuer = Role "Borrower"

principal :: Value
principal = ConstantParam "Principal"

instalment :: Value
instalment = ConstantParam "Interest instalment"

guaranteedAmount :: BigInt -> Value
guaranteedAmount instalments = AddValue
  (MulValue (Constant instalments) instalment)
  principal

lastInstalment :: Value
lastInstalment = AddValue instalment principal

deposit
  :: Value -> Party -> Party -> Timeout -> Contract -> Contract -> Contract
deposit amount by toAccount timeout timeoutContinuation continuation =
  When [ Case (Deposit toAccount by ada amount) continuation ]
    timeout
    timeoutContinuation

refundGuarantor :: Value -> Contract -> Contract
refundGuarantor amount continuation = Pay investor (Party guarantor) ada amount
  continuation

transfer
  :: Value -> Party -> Party -> Timeout -> Contract -> Contract -> Contract
transfer amount from to timeout timeoutContinuation continuation =
  deposit amount from to timeout timeoutContinuation
    $ Pay to (Party to) ada amount
        continuation

contract :: Contract
contract =
  deposit (guaranteedAmount (fromInt 3)) guarantor investor
    -- TODO: SCP-3887 unify time construct
    (TimeValue $ POSIXTime $ unsafeInstantFromInt 300)
    Close
    $ transfer principal investor issuer
        (TimeValue $ POSIXTime $ unsafeInstantFromInt 600)
        (refundGuarantor (guaranteedAmount (fromInt 3)) Close)
    $ transfer instalment issuer investor
        (TimeValue $ POSIXTime $ unsafeInstantFromInt 900)
        Close
    $ refundGuarantor instalment
    $ transfer instalment issuer investor
        (TimeValue $ POSIXTime $ unsafeInstantFromInt 1200)
        Close
    $ refundGuarantor instalment
    $ transfer lastInstalment issuer investor
        (TimeValue $ POSIXTime $ unsafeInstantFromInt 1500)
        Close
    $ refundGuarantor lastInstalment
        Close
