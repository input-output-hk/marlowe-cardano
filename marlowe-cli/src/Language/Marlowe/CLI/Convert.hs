{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------

-- | Convert Marlowe contracts.
module Language.Marlowe.CLI.Convert (
  maybeWritePretty,
  maybeWriteJson,
  readContractPretty,
  readContractJson,
) where

import Control.Monad.Combinators as C
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BS (getContents, pack)
import Data.Functor (($>))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Void
import GHC.Generics (Generic (..))
import Language.Marlowe.CLI.IO (decodeFileStrict, maybeWriteJson)
import Language.Marlowe.CLI.Types (CliError (..))
import Language.Marlowe.Core.V1.Semantics.Types as S (
  AccountId,
  Action (..),
  Bound (..),
  Case (..),
  ChoiceId (..),
  Contract (..),
  CurrencySymbol (..),
  Observation (..),
  Party (..),
  Payee (..),
  Timeout,
  Token (..),
  TokenName (..),
  Value (..),
  ValueId (..),
 )
import Language.Marlowe.Core.V1.Semantics.Types.Address
import Language.Marlowe.Pretty
import Plutus.V1.Ledger.Time (POSIXTime (..))
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Builtins.Class (ToBuiltin (..))
import Text.Megaparsec
import Text.Megaparsec.Char as P
import Text.Megaparsec.Char.Lexer qualified as L

-- | Read a Marlowe Contract as JSON from a file.
readContractJson
  :: (MonadError CliError m)
  => (MonadIO m)
  => Maybe FilePath
  -> m Contract
readContractJson (Just inputFile) = decodeFileStrict inputFile
readContractJson Nothing =
  do
    result :: Either String Contract <- eitherDecodeStrict <$> liftIO BS.getContents
    liftEither $ first (CliError . show) result

-- | Read a pretty printed Marlowe Contract from a file.
readContractPretty
  :: (MonadError CliError m)
  => (MonadIO m)
  => Maybe FilePath
  -> m Contract
readContractPretty (Just inputFile) = do
  result <- parseFromFile inputFile
  liftEither $ first (CliError . errorBundlePretty) result
readContractPretty Nothing = do
  result <- parseFromStdin
  liftEither $ first (CliError . errorBundlePretty) result

parseFromFile
  :: (MonadIO m)
  => String
  -> m (Either (ParseErrorBundle Text Void) Contract)
parseFromFile f = runParserT contractParser f =<< liftIO (T.readFile f)

parseFromStdin
  :: (MonadIO m)
  => m (Either (ParseErrorBundle Text Void) Contract)
parseFromStdin = runParserT contractParser "stdin" =<< liftIO T.getContents

-- | Optional write a pretty printed Marlowe contract to a file, otherwise write to standard output.
maybeWritePretty
  :: (MonadIO m)
  => (Generic a)
  => (Pretty1 (Rep a))
  => Maybe FilePath
  -> a
  -> m ()
maybeWritePretty Nothing = liftIO . print . pretty
maybeWritePretty (Just outputFile) = liftIO . writeFile outputFile . show . pretty

contractParser
  :: ParsecT Void Text m Contract
contractParser = contractParser' <|> parens contractParser
  where
    contractParser' =
      closeParser
        <|> payParser
        <|> ifParser
        <|> whenParser
        <|> letParser
        <|> assertParser
      where
        closeParser =
          symbol "Close"
            $> Close
        payParser =
          symbol "Pay"
            >> Pay
              <$> accountIdParser
              <*> payeeParser
              <*> tokenParser
              <*> valueParser
              <*> contractParser
        ifParser =
          symbol "If"
            >> If
              <$> observationParser
              <*> contractParser
              <*> contractParser
        whenParser =
          symbol "When"
            >> When
              <$> brackets ((spaceConsumer >> caseParser) `sepBy` char ',')
              <*> timeoutParser
              <*> contractParser
        letParser =
          symbol "Let"
            >> Let
              <$> valueIdParser
              <*> valueParser
              <*> contractParser
        assertParser =
          symbol "Assert"
            >> Assert
              <$> observationParser
              <*> contractParser

caseParser :: ParsecT Void Text m (Case Contract)
caseParser =
  symbol "Case"
    >> Case
      <$> actionParser
      <*> contractParser

actionParser :: ParsecT Void Text m Action
actionParser =
  parens $
    depositParser
      <|> choiceParser
      <|> notifyParser
  where
    depositParser =
      symbol "Deposit"
        >> Deposit
          <$> accountIdParser
          <*> partyParser
          <*> tokenParser
          <*> valueParser
    choiceParser =
      symbol "Choice"
        >> Choice
          <$> choiceIdParser
          <*> brackets ((spaceConsumer >> boundParser) `sepBy` char ',')
    notifyParser =
      symbol "Notify"
        >> Notify
          <$> observationParser

boundParser :: ParsecT Void Text m Bound
boundParser =
  symbol "Bound"
    >> Bound
      <$> integer
      <*> integer

valueParser :: ParsecT Void Text m (Value Observation)
valueParser = valueParser' <|> parens valueParser
  where
    valueParser' =
      availableMoneyParser
        <|> constantParser
        <|> negValueParser
        <|> addValueParser
        <|> subValueParser
        <|> mulValueParser
        <|> divValueParser
        <|> choiceValueParser
        <|> timeIntervalStartParser
        <|> timeIntervalEndParser
        <|> useValueParser
        <|> condParser
    availableMoneyParser =
      symbol "AvailableMoney"
        >> AvailableMoney
          <$> accountIdParser
          <*> tokenParser
    constantParser =
      symbol "Constant"
        >> Constant
          <$> integer
    negValueParser =
      symbol "NegValue"
        >> NegValue
          <$> valueParser
    addValueParser =
      symbol "AddValue"
        >> AddValue
          <$> valueParser
          <*> valueParser
    subValueParser =
      symbol "SubValue"
        >> SubValue
          <$> valueParser
          <*> valueParser
    mulValueParser =
      symbol "MulValue"
        >> MulValue
          <$> valueParser
          <*> valueParser
    divValueParser =
      symbol "DivValue"
        >> DivValue
          <$> valueParser
          <*> valueParser
    choiceValueParser =
      symbol "NegValue"
        >> ChoiceValue
          <$> choiceIdParser
    timeIntervalStartParser =
      symbol "NegValue"
        $> TimeIntervalStart
    timeIntervalEndParser =
      symbol "NegValue"
        $> TimeIntervalEnd
    useValueParser =
      symbol "NegValue"
        >> UseValue
          <$> valueIdParser
    condParser =
      symbol "NegValue"
        >> Cond
          <$> observationParser
          <*> valueParser
          <*> valueParser

observationParser :: ParsecT Void Text m Observation
observationParser = observationParser' <|> parens observationParser
  where
    observationParser' =
      andObsParser
        <|> orObsParser
        <|> notObsParser
        <|> choseSomethingParser
        <|> valueGEParser
        <|> valueGTParser
        <|> valueLTParser
        <|> valueLEParser
        <|> valueEQParser
        <|> notObsParser
        <|> trueObsParser
        <|> falseObsParser
    andObsParser =
      symbol "AndObs"
        >> AndObs
          <$> observationParser
          <*> observationParser
    orObsParser =
      symbol "OrObs"
        >> OrObs
          <$> observationParser
          <*> observationParser
    notObsParser =
      symbol "NotObs"
        >> NotObs
          <$> observationParser
    choseSomethingParser =
      symbol "ChoseSomething"
        >> ChoseSomething
          <$> choiceIdParser
    valueGEParser =
      symbol "ValueGE"
        >> ValueGE
          <$> valueParser
          <*> valueParser
    valueGTParser =
      symbol "ValueGT"
        >> ValueGT
          <$> valueParser
          <*> valueParser
    valueLTParser =
      symbol "ValueLT"
        >> ValueLT
          <$> valueParser
          <*> valueParser
    valueLEParser =
      symbol "ValueLE"
        >> ValueLE
          <$> valueParser
          <*> valueParser
    valueEQParser =
      symbol "ValueEQ"
        >> ValueEQ
          <$> valueParser
          <*> valueParser
    trueObsParser =
      symbol "TrueObs"
        $> TrueObs
    falseObsParser =
      symbol "FalseObs"
        $> FalseObs

partyParser :: ParsecT Void Text m Party
partyParser =
  parens $
    addressParser
      <|> roleParser
  where
    addressParser =
      symbol "Address" >> toAddr
    roleParser =
      symbol "Role"
        >> Role
          <$> tokenNameParser
    toAddr = do
      addr <- fromString <$> stringLiteral
      let p = deserialiseAddressBech32 addr
      case p of
        Just (b, a) -> return $ Address b a
        Nothing -> customFailure undefined

accountIdParser :: ParsecT Void Text m AccountId
accountIdParser = partyParser

choiceIdParser :: ParsecT Void Text m ChoiceId
choiceIdParser =
  parens $
    symbol "ChoiceId"
      >> ChoiceId
        <$> quotedBuiltinByteString
        <*> partyParser

valueIdParser :: ParsecT Void Text m ValueId
valueIdParser =
  parens $
    symbol "ValueId"
      >> ValueId
        <$> quotedBuiltinByteString

tokenParser :: ParsecT Void Text m S.Token
tokenParser =
  parens $
    symbol "Token"
      >> S.Token
        <$> currencySymbolParser
        <*> tokenNameParser

currencySymbolParser :: ParsecT Void Text m S.CurrencySymbol
currencySymbolParser =
  lexeme $
    S.CurrencySymbol
      <$> quotedBuiltinByteString

tokenNameParser :: ParsecT Void Text m TokenName
tokenNameParser =
  lexeme $
    TokenName
      <$> quotedBuiltinByteString

payeeParser :: ParsecT Void Text m Payee
payeeParser =
  parens $
    (symbol "Acccount" >> Account <$> accountIdParser)
      <|> (symbol "Party" >> Party <$> partyParser)

timeoutParser :: ParsecT Void Text m Timeout
timeoutParser =
  lexeme $
    POSIXTime
      <$> integer

integer :: ParsecT Void Text m Integer
integer = lexeme L.decimal

parens :: ParsecT Void Text m a -> ParsecT Void Text m a
parens = between (symbol "(") (symbol ")")

brackets :: ParsecT Void Text m a -> ParsecT Void Text m a
brackets = between (symbol "[") (symbol "]")

stringLiteral :: ParsecT Void Text m String
stringLiteral = P.char '"' >> C.manyTill L.charLiteral (P.char '"')

quotedBuiltinByteString :: ParsecT Void Text m BuiltinByteString
quotedBuiltinByteString = lexeme $ toBuiltin . BS.pack <$> stringLiteral

lexeme :: ParsecT Void Text m a -> ParsecT Void Text m a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> ParsecT Void Text m Text
symbol = L.symbol spaceConsumer

spaceConsumer :: ParsecT Void Text m ()
spaceConsumer = L.space space1 empty empty
