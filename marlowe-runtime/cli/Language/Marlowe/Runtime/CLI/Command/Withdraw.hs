module Language.Marlowe.Runtime.CLI.Command.Withdraw
  where

import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand, txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.CLI.Option (txOutRefParser)
import Language.Marlowe.Runtime.ChainSync.Api (TokenName(TokenName))
import Language.Marlowe.Runtime.Core.Api (ContractId(ContractId))
import Options.Applicative

data WithdrawCommand = WithdrawCommand
  { contractId :: ContractId
  , role :: TokenName
  }

withdrawCommandParser :: ParserInfo (TxCommand WithdrawCommand)
withdrawCommandParser = info (txCommandParser parser) $ progDesc "Withdraw funds paid to a role in a contract"
  where
    parser = WithdrawCommand <$> contractIdParser <*> roleParser
    contractIdParser = option (ContractId <$> txOutRefParser) $ mconcat
      [ long "contract"
      , short 'c'
      , metavar "CONTRACT_ID"
      , help "The ID of the Marlowe contract from which to withdraw funds."
      ]
    roleParser = fmap TokenName $ strOption $ mconcat
      [ long "role"
      , metavar "ROLE_NAME"
      , help "The name of the role from which to withdraw funds."
      ]


runWithdrawCommand :: TxCommand WithdrawCommand -> CLI ()
runWithdrawCommand = error "not implemented"
