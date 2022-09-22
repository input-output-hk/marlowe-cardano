-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests using external semantics oracle.
--
-----------------------------------------------------------------------------



{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Semantics.Oracle
  ( -- * Tests
    tests
  ) where


import Data.Aeson
  ( FromJSON(parseJSON)
  , KeyValue((.=))
  , ToJSON(toJSON)
  , Value(Array)
  , eitherDecode
  , encode
  , object
  , withArray
  , withObject
  , (.:)
  )
import Data.Aeson.Types (Parser)
import Data.Function (on)
import Data.List (sort)
import Data.String (fromString)
import Data.Vector ((!))
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics
  ( Payment(..)
  , TransactionInput
  , TransactionOutput(TransactionOutput, txOutContract, txOutPayments, txOutState, txOutWarnings)
  , computeTransaction
  )
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State(..), Token(Token))
import Language.Marlowe.FindInputs (getAllInputs)
import System.Directory (doesFileExist)
import System.IO
  (BufferMode(LineBuffering), Handle, IOMode(ReadMode, WriteMode), hGetLine, hPutStrLn, hSetBuffering, openFile)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCaseSteps)

import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, unpack)
import qualified Data.Vector as V (fromList)
import qualified Language.Marlowe.Extended.V1 as E (Contract(Close), Value(Constant), toCore)
import qualified PlutusTx.AssocMap as AM (Map, empty, toList)

import qualified Marlowe.Contracts.Escrow as Escrow (escrow)
import qualified Marlowe.Contracts.Forward as Forward (forward)
import qualified Marlowe.Contracts.Futures as Futures (future)
import qualified Marlowe.Contracts.Swap as Swap (swap)
import qualified Marlowe.Contracts.Trivial as Trivial (trivial)
import qualified Marlowe.Contracts.ZeroCouponBond as ZeroCouponBond (zeroCouponBond)


-- | Compare contract execution to the semantics oracle.
--
-- The function reads from a named pipe "orin" and writes to a
-- named pipe "orout". Those same pipes must be connected to the
-- semantics oracle. For example:
--
-- > git clone git@github.com:input-output-hk/marlowe-cardano.git -b 472170bdf77dc9d89bc77861da851d2476b2aa47
-- > nix-shell
-- > cd marlowe-purescript-spec
-- > spago install
-- > spago build
-- > npm install
-- > node index.js <orout >orin
tests :: TestTree
tests =
  testCaseSteps "Semantics Oracle"
    $ \step ->
      do
      okay <- and <$> mapM doesFileExist ["orin", "orout"]
      if okay
        then do
               step "Connecting to semantics oracle . . ."
               hToOracle   <- openFile "orout" WriteMode
               hFromOracle <- openFile "orin"  ReadMode
               hSetBuffering hToOracle   LineBuffering
               hSetBuffering hFromOracle LineBuffering
               sequence_
                 [
                   do
                     step $ "Comparing " <> name <> " to oracle . . ."
                     compareToOracle hToOracle hFromOracle name contract
                 |
                   (name, contract) <- contracts
                 ]
        else step "No semantics oracle found on pipes \"orin\" and \"orout\"."


-- | Contracts to be tested.
contracts :: [(String, E.Contract)]
contracts =
  let
    alice = "Alice"
    bob = "Bob"
    charlie = "Charlie"
    v = E.Constant
    a = Token "aaaa" "A"
    b = Token "bbbb" "B"
    c = Token "cccc" "C"
  in
    [
      ("Escrow"          , Escrow.escrow (v 900_000_000) alice bob charlie 100 200 300 400)
    , ("Forward"         , Forward.forward alice a (v 10) 100 bob b (v 20) 200 300)
    , ("Future"          , Futures.future alice bob (v 100) (v 200) 100 [200] 500)
    , ("Swap"            , Swap.swap alice a (v 100) 100 bob b (v 200) 200 E.Close)
    , ("Trivial"         , Trivial.trivial alice 100_000_000 70_000_000 0)
    , ("Zero-Coupon Bond", ZeroCouponBond.zeroCouponBond alice bob 100 200 (v 100_000_000) (v 200_000_000) c E.Close)
    ]


-- | A request to the semantics oracle.
data OracleRequest =
  OracleRequest
  {
    request          :: String
  , state            :: State
  , transactionInput :: TransactionInput
  , coreContract     :: Contract
  }
  deriving (Generic, Show)

instance FromJSON OracleRequest

instance ToJSON OracleRequest


-- | A response from the semantics oracle.
data OracleResponse =
    TransactionSuccess TransactionOutput
  | TransactionError
  deriving (Generic, Show)

instance FromJSON OracleResponse where
  parseJSON =
    withObject "OracleResponse"
      $ \o ->
        do
          success <- o .: "transaction_success"
          txOutWarnings <- success .: "txOutWarnings"
          payments      <- success .: "txOutPayments"
          txOutState    <- success .: "txOutState"
          txOutContract <- success .: "txOutContract"
          txOutPayments <- mapM paymentFromJSON payments
          pure . TransactionSuccess $ TransactionOutput{..}


instance ToJSON OracleResponse where
  toJSON (TransactionSuccess TransactionOutput{..}) =
    object
      [
        "transaction_success" .= object
                                   [
                                     "txOutWarnings" .= toJSON txOutWarnings
                                   , "txOutPayments" .= fmap paymentToJSON txOutPayments
                                   , "txOutState"    .= toJSON txOutState
                                   , "txOutContract" .= toJSON txOutContract
                                   ]
      ]
  toJSON _ = "transaction_error"


-- | Deserialize a payment from JSON.
paymentFromJSON :: Value -> Parser Payment
paymentFromJSON =
  withArray "Payment"
    $ \v ->
      do
        a <- parseJSON $ v ! 0
        p <- parseJSON $ v ! 1
        [(c, [(n, i)])] <- parseJSON (v ! 2)
        pure $ Payment a p (Token (fromString c) (fromString n)) i


-- | Serialize a payment to JSON.
paymentToJSON :: Payment -> Value
paymentToJSON (Payment a p t i) =
  Array
    $ V.fromList
    [
      toJSON a
    , toJSON p
    , toJSON t
    , toJSON i
    ]


-- | Equality test for transaction output.
eqTransactionOutput :: TransactionOutput -> TransactionOutput -> Bool
eqTransactionOutput x@TransactionOutput{} y@TransactionOutput{} =
     txOutWarnings x == txOutWarnings y
  && txOutPayments x == txOutPayments y
  && txOutState    x ~~ txOutState    y
  && txOutContract x == txOutContract y
eqTransactionOutput _ _ = False


-- | Equality test for state.
(~~) :: State -> State -> Bool
x ~~ y =
     accounts    x `eqAssocMap` accounts    y
  && choices     x `eqAssocMap` choices     y
  && boundValues x `eqAssocMap` boundValues y
  && minTime     x ==           minTime     y


-- | Equality test for association lists.
eqAssocMap :: Ord k => Ord v => AM.Map k v -> AM.Map k v -> Bool
eqAssocMap = (==) `on` (sort . AM.toList)


-- | Use static analysis to generate paths through a contract, and then compare execution to the oracle.
compareToOracle :: Handle -> Handle -> String -> E.Contract -> IO ()
compareToOracle hToOracle hFromOracle name extended =
  do
    Just core <- pure $ E.toCore extended
    Right paths <- getAllInputs core
    let
      accounts = AM.empty
      choices = AM.empty
      boundValues = AM.empty
      request = "compute-transaction"
    sequence_
      [
        let
          go _ _ _ [] = pure ()
          go j state coreContract (transactionInput : transactionInputs) =
            do
              hPutStrLn hToOracle "```"
              hPutStrLn hToOracle $ LBS8.unpack $ encode OracleRequest{..}
              hPutStrLn hToOracle "```"
              result <- hGetLine hFromOracle
              actual@TransactionOutput{..} <- pure $ computeTransaction transactionInput state coreContract
              Right (TransactionSuccess expected) <- pure $ eitherDecode $ LBS8.pack result
              let match = actual `eqTransactionOutput` expected
              assertBool (name <> ", path " <> show (i :: Int) <> ", step " <> show (j :: Int)) match
              go (j + 1) txOutState txOutContract transactionInputs
        in
          go 1 State{..} core inputs
      |
        (i, (minTime, inputs)) <- zip [1..] paths
      ]
