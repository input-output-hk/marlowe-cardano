{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.API.Command.Withdraw
  where

import qualified Cardano.Api as C
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Aeson (toJSON)
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Language.Marlowe.Runtime.API.Command.Tx (SigningMethod(Manual), TxCommand(..), txCommandParser)
import Language.Marlowe.Runtime.API.Monad (CLI, runCLIExceptT, runTxCommand)
import Language.Marlowe.Runtime.CLI.Option (marloweVersionParser, txOutRefParser)
import Language.Marlowe.Runtime.ChainSync.Api (TokenName(TokenName))
import Language.Marlowe.Runtime.Core.Api
  (ContractId(ContractId), MarloweVersion(MarloweV1), MarloweVersionTag(V1), SomeMarloweVersion(SomeMarloweVersion))
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand(Withdraw), WithdrawError)
import Options.Applicative

data WithdrawCommand = WithdrawCommand
  { contractId :: ContractId
  , marloweVersion :: SomeMarloweVersion
  , role :: TokenName
  }

data WithdrawCommandError v
  = WithdrawFailed (WithdrawError v)
  | TransactionFileWriteFailed (C.FileError ())

deriving instance Show (WithdrawCommandError 'V1)

withdrawCommandParser :: ParserInfo (TxCommand WithdrawCommand)
withdrawCommandParser = info (txCommandParser parser) $ progDesc "Withdraw funds paid to a role in a contract"
  where
    parser = WithdrawCommand <$> contractIdParser <*> marloweVersionParser <*> roleParser
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
runWithdrawCommand TxCommand { walletAddresses, signingMethod, subCommand=WithdrawCommand{..}} = case marloweVersion of
  SomeMarloweVersion MarloweV1 -> runCLIExceptT do
    let
      cmd = Withdraw MarloweV1 walletAddresses contractId role
    txBody <- ExceptT $ first WithdrawFailed <$> runTxCommand cmd
    case signingMethod of
      Manual outputFile -> do
        ExceptT $ liftIO $ first TransactionFileWriteFailed <$> C.writeFileTextEnvelope outputFile Nothing txBody
        let
          txId = C.getTxId txBody
          res = A.object
            [ ("txId", toJSON txId) ]
        liftIO . print $ A.encode res


