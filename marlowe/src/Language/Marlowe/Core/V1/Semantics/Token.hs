{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Tokens as used on the Cardano blockchain
--
-- Intended for unqualified import.
module Language.Marlowe.Core.V1.Semantics.Token (
    Token(..)
  , moneyToValue
  , moneyFromValue
  ) where

import PlutusTx.Prelude hiding ((<$>), (<*>))
import Prelude ((<$>), (<*>))
import qualified Prelude as Haskell

import Data.Aeson.Types hiding (Error, Value)
import Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import GHC.Generics
import Language.Marlowe.Core.V1.Semantics.Money
import Language.Marlowe.Pretty (Pretty (..))
import Ledger (Value)
import Ledger.Value (CurrencySymbol (..), TokenName (..))
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Lift (makeLift)

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Extras as JSON
import qualified Ledger.Value as Val
import qualified PlutusTx.AssocMap as Map

{-| Token - represents a currency or token, it groups
    a pair of a currency symbol and token name.
-}
data Token = Token CurrencySymbol TokenName
  deriving stock (Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)

instance Haskell.Show Token where
  showsPrec p (Token cs tn) =
    Haskell.showParen (p Haskell.>= 11) (Haskell.showString $ "Token \"" Haskell.++ Haskell.show cs Haskell.++ "\" " Haskell.++ Haskell.show tn)

instance FromJSON Token where
  parseJSON = withObject "Token" (\v ->
       Token <$> (Val.currencySymbol <$> (JSON.decodeByteString =<< (v .: "currency_symbol")))
             <*> (Val.tokenName . Text.encodeUtf8 <$> (v .: "token_name"))
                                 )

instance ToJSON Token where
  toJSON (Token currSym tokName) = object
      [ "currency_symbol" .= (JSON.String $ JSON.encodeByteString $ fromBuiltin $ unCurrencySymbol currSym)
      , "token_name" .= (JSON.String $ Text.decodeUtf8 $ fromBuiltin $ unTokenName tokName)
      ]

instance Eq Token where
    {-# INLINABLE (==) #-}
    (Token n1 p1) == (Token n2 p2) = (n1, p1) == (n2, p2)

instance Ord Token where
    {-# INLINABLE compare #-}
    (Token n1 p1) `compare` (Token n2 p2) = (n1, p1) `compare` (n2, p2)

moneyToValue :: Money Token -> Value
moneyToValue (Money m) =
    mconcat $ map aux $ Map.toList m
  where
    aux :: (Token, Integer) -> Value
    aux (Token symbol name, n) = Val.singleton symbol name n

moneyFromValue :: Value -> Money Token
moneyFromValue = Money . Map.fromList . map aux . Val.flattenValue
  where
    aux :: (CurrencySymbol, TokenName, Integer) -> (Token, Integer)
    aux (symbol, name, n) = (Token symbol name, n)

makeLift ''Token
makeIsDataIndexed ''Token [('Token,0)]

