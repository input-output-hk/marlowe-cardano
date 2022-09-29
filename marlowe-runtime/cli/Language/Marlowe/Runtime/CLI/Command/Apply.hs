{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Marlowe.Runtime.CLI.Command.Apply
  where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Language.Marlowe (POSIXTime(..))
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand, txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.CLI.Option (txOutRefParser)
import Language.Marlowe.Runtime.ChainSync.Api (unPolicyId)
import Language.Marlowe.Runtime.Core.Api (ContractId(..), IsMarloweVersion(..), MarloweVersionTag(..))
import qualified Language.Marlowe.Util as V1
import Options.Applicative
import qualified Plutus.V1.Ledger.Api as P

data ApplyCommand = V1ApplyCommand
  { contractId :: ContractId
  , inputs :: ContractInputs 'V1
  , validityLowerBound :: Maybe POSIXTime
  , validityUpperBound :: Maybe POSIXTime
  }

data ContractInputs v
  = ContractInputsByFile FilePath
  | ContractInputsByValue (Redeemer v)
  | ContractInputsByValueWithContinuations (Redeemer v) [FilePath]

applyCommandParser :: ParserInfo (TxCommand ApplyCommand)
applyCommandParser = info (txCommandParser parser) $ progDesc "Apply inputs to a contract"
  where
    parser = V1ApplyCommand
      <$> contractIdParser "which to apply the inputs"
      <*> (ContractInputsByFile <$> inputsFileParser)
      <*> validityLowerBoundParser
      <*> validityUpperBoundParser

    inputsFileParser = strOption $ mconcat
      [ long "inputs-file"
      , help "A file containing the Marlowe inputs to apply to the contract in JSON format."
      , metavar "FILE_PATH"
      ]

depositCommandParser :: ParserInfo (TxCommand ApplyCommand)
depositCommandParser = singleInputParser "deposit funds into" contentParser $ progDesc "Deposit funds into a contract"
  where
    contentParser = do
      accountId <- accountIdParser
      party <- partyParser
      (token, quantity) <- tokenParser <|> lovelaceParser
      pure $ V1.IDeposit accountId party token quantity

    accountIdParser = toRoleParser <|> toAddressParser
    toRoleParser = roleOption $ mconcat
      [ long "to-role"
      , metavar "ROLE_NAME"
      , help "The name of the role into whose account to deposit the funds."
      ]
    toAddressParser = addressOption $ mconcat
      [ long "to-address"
      , metavar "ADDRESS"
      , help "The address into whose account to deposit the funds."
      ]
    partyParser = fromRoleParser <|> fromAddressParser
    fromRoleParser = roleOption $ mconcat
      [ long "from-role"
      , metavar "ROLE_NAME"
      , help "The name of the role depositing the funds."
      ]
    fromAddressParser = addressOption $ mconcat
      [ long "from-address"
      , metavar "ADDRESS"
      , help "The address depositing the funds."
      ]
    lovelaceParser = option ((V1.ada,) <$> auto) $ mconcat
      [ long "lovelace"
      , short 'l'
      , help "The quantity of lovelace to deposit."
      , metavar "INTEGER"
      ]
    tokenParser = (,) <$> (V1.Token <$> currencySymbolParser <*> tokenNameParser) <*> quantityParser
    currencySymbolParser = fmap (P.CurrencySymbol . P.toBuiltin . unPolicyId) $ strOption $ mconcat
      [ long "currency"
      , short 'c'
      , metavar "MINTING_POLICY_ID"
      , help "The minting policy ID of the token(s) to deposit."
      ]
    tokenNameParser = fmap (P.TokenName . P.toBuiltin @ByteString) $ strOption $ mconcat
      [ long "token-name"
      , short 'n'
      , metavar "TOKEN_NAME"
      , help "The name of the token(s) to deposit."
      ]
    quantityParser = option auto $ mconcat
      [ long "quantity"
      , short 'q'
      , metavar "INTEGER"
      , help "The quantity of tokens to deposit."
      ]

chooseCommandParser :: ParserInfo (TxCommand ApplyCommand)
chooseCommandParser = singleInputParser "make a choice in." contentParser $ progDesc "Notify a contract to proceed"
  where
    contentParser = do
      choiceId <- choiceIdParser
      chosenNum <- chosenNumParser
      pure $ V1.IChoice choiceId chosenNum
    choiceIdParser = V1.ChoiceId <$> choiceIdNameParser <*> choiceIdPartyParser
    choiceIdNameParser = strOption $ mconcat
      [ long "choice"
      , metavar "CHOICE_NAME"
      , help "The name of the choice being made."
      ]
    choiceIdPartyParser = choiceIdPartyRoleParser <|> choiceIdPartyAddressParser
    choiceIdPartyRoleParser = roleOption $ mconcat
      [ long "as-role"
      , metavar "ROLE_NAME"
      , help "Make the choice as the specified role."
      ]
    choiceIdPartyAddressParser = addressOption $ mconcat
      [ long "as-address"
      , metavar "ADDRESS"
      , help "Make the choice as the specified address."
      ]
    chosenNumParser = option auto $ mconcat
      [ long "value"
      , metavar "INTEGER"
      , help "The value being chosen."
      ]

notifyCommandParser :: ParserInfo (TxCommand ApplyCommand)
notifyCommandParser = singleInputParser "notify" (pure V1.INotify) $ progDesc "Notify a contract to proceed"

singleInputParser :: String -> Parser V1.InputContent -> InfoMod (TxCommand ApplyCommand) -> ParserInfo (TxCommand ApplyCommand)
singleInputParser verb contentParser = info (txCommandParser parser)
  where
    parser = V1ApplyCommand
      <$> contractIdParser verb
      <*> inputParser
      <*> validityLowerBoundParser
      <*> validityUpperBoundParser
    inputParser = do
      content <- V1.NormalInput <$> contentParser
      mContinuation <- optional continuationParser
      pure case mContinuation of
        Nothing -> ContractInputsByValue [content]
        Just continuationFile -> ContractInputsByValueWithContinuations [content] [continuationFile]
    continuationParser = strOption $ mconcat
      [ long "continuation-file"
      , help "A file containing the continuation contract JSON for making a choice in a Merkleized contract."
      , metavar "FILE_PATH"
      ]

contractIdParser :: String -> Parser ContractId
contractIdParser verb = option (ContractId <$> txOutRefParser) $ mconcat
  [ long "contract"
  , short 'c'
  , metavar "CONTRACT_ID"
  , help $ "The ID of the Marlowe contract to " <> verb
  ]

validityLowerBoundParser :: Parser (Maybe POSIXTime)
validityLowerBoundParser = optional $ option readPOSIXTime $ mconcat
  [ long "validity-lower-bound"
  , short 'l'
  , metavar "TIMESTAMP"
  , help "The lower bound of the transaction validity interval in POSIX milliseconds. If not specified, the current time (as determined by the Cardano node) will be used."
  ]

-- Read an integer number of milliseconds from the UNIX epoch
readPOSIXTime :: ReadM POSIXTime
readPOSIXTime = POSIXTime <$> auto

validityUpperBoundParser :: Parser (Maybe POSIXTime)
validityUpperBoundParser = optional $ option readPOSIXTime $ mconcat
  [ long "validity-upper-bound"
  , short 'u'
  , metavar "TIMESTAMP"
  , help "The upper bound of the transaction validity interval in POSIX milliseconds. If not specified, the next timeout in the contract will be used (bounded by the maximum value allowed by the Cardano node)."
  ]

roleOption :: Mod OptionFields P.BuiltinByteString -> Parser V1.Party
roleOption = fmap (V1.Role . P.TokenName) . strOption

addressOption :: Mod OptionFields V1.Party -> Parser V1.Party
addressOption = option readAddress

readAddress :: ReadM V1.Party
readAddress = maybeReader $ fmap (uncurry V1.Address) . V1.deserialiseAddressBech32 . T.pack

runApplyCommand :: TxCommand ApplyCommand -> CLI ()
runApplyCommand = error "not implemented"
