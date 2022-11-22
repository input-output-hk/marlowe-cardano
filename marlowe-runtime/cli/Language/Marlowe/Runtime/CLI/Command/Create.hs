{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.CLI.Command.Create
  where

import qualified Cardano.Api as C
import Control.Error.Util (hoistMaybe, noteT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), throwE)
import Data.Aeson (toJSON)
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import qualified Data.Yaml as Yaml
import Data.Yaml.Aeson (decodeFileEither)
import Language.Marlowe (POSIXTime(POSIXTime))
import Language.Marlowe.Runtime.CLI.Command.Tx (SigningMethod(Manual), TxCommand(..), txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI, runCLIExceptT, runTxCommand)
import Language.Marlowe.Runtime.CLI.Option (keyValueOption, marloweVersionParser, parseAddress)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , Lovelace(Lovelace)
  , PolicyId
  , TokenName(..)
  , TransactionMetadata
  , fromJSONEncodedTransactionMetadata
  , renderTxOutRef
  )
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(ContractId)
  , IsMarloweVersion(Contract)
  , MarloweVersion(MarloweV1)
  , MarloweVersionTag(V1)
  , SomeMarloweVersion(SomeMarloweVersion)
  )
import Language.Marlowe.Runtime.Transaction.Api (CreateError, MarloweTxCommand(Create), RoleTokensConfig(..), mkMint)
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import Text.Read (readMaybe)

data CreateCommand = CreateCommand
  { marloweVersion :: SomeMarloweVersion
  , roles :: Maybe RolesConfig
  , contractFiles :: ContractFiles
  , minUTxO :: Lovelace
  }

data RolesConfig
  = MintSimple (NonEmpty (TokenName, Address))
  | MintConfig FilePath
  | UseExistingPolicyId PolicyId
  deriving (Show)

data ContractFiles
  = CoreFile FilePath
  | ExtendedFiles FilePath ContractArgs

data ContractArgs
  = ContractArgsByFile FilePath
  | ContractArgsByValue ContractArgsValue

data ContractArgsValue = ContractArgsValue
  { timeoutArguments :: Map String POSIXTime
  , valueArguments :: Map String Integer
  }

data CreateCommandError v
  = CreateFailed (CreateError v)
  | ContractFileDecodingError Yaml.ParseException
  | TransactionFileWriteFailed (C.FileError ())
  | RolesConfigNotSupportedYet RolesConfig
  | MetadataDecodingFailed (Maybe Yaml.ParseException)
  | ExtendedContractsAreNotSupportedYet

deriving instance Show (CreateCommandError 'V1)

createCommandParser :: ParserInfo (TxCommand CreateCommand)
createCommandParser = info (txCommandParser parser) $ progDesc "Create a new Marlowe Contract"
  where
    parser = CreateCommand
      <$> marloweVersionParser
      <*> rolesParser
      <*> contractFilesParser
      <*> minUTxOParser
    rolesParser = optional (mintSimpleParser <|> mintConfigParser <|> policyIdParser)
    mintSimpleParser = MintSimple <$> some1 roleParser
    mintConfigParser = fmap MintConfig $ strOption $ mconcat
      [ long "roles-config-file"
      , help $ unwords
          [ "A JSON file containing a map of role token names to a roles configuration object."
          , "The roles configuration object has two keys, \"address\" and \"metadata\","
          , "where \"address\" is the address to send the newly minted role token and \"metadata\""
          , "is the CIP-25 metadata object to associate with the token."
          ]
      , metavar "FILE_PATH"
      ]
    policyIdParser = fmap UseExistingPolicyId $ strOption $ mconcat
      [ long "role-token-policy-id"
      , metavar "POLICY_ID"
      , help "The hexadecimal-encoded policy ID of the role tokens for this contract. This option is used to support role tokens minted in a separate transaction."
      ]
    roleParser = keyValueOption (Right . TokenName . fromString) parseAddress $ mconcat
      [ long "role"
      , short 'r'
      , help "The name of a role in the contract with the address to send the token to"
      , metavar "ROLE=ADDRESS"
      ]
    contractFilesParser = CoreFile <$> coreParser <|> extendedParser
    coreParser = strOption $ mconcat
      [ long "core-file"
      , help "A file containing the Core Marlowe JSON definition of the contract to create."
      , metavar "FILE_PATH"
      ]
    extendedParser = ExtendedFiles <$> extendedFileParser <*> contractArgsParser
    extendedFileParser = strOption $ mconcat
      [ long "contract-file"
      , help "A file containing the Extended Marlowe JSON definition of the contract to create."
      , metavar "FILE_PATH"
      ]
    contractArgsParser =
      ContractArgsByFile <$> argsFileParser <|> ContractArgsByValue <$> contractArgsValueParser
    argsFileParser = strOption $ mconcat
      [ long "args-file"
      , help "A file containing the Extended Marlowe arguments to apply to the contract."
      , metavar "FILE_PATH"
      ]
    contractArgsValueParser = ContractArgsValue
      <$> timeoutArgumentsParser
      <*> valueArgumentsParser
    timeoutArgumentsParser = Map.fromList <$> many timeoutArgumentParser
    valueArgumentsParser = Map.fromList <$> many valueArgumentParser
    timeoutArgumentParser = keyValueOption Right (fmap POSIXTime . integerParser) $ mconcat
      [ long "timeout-arg"
      , metavar "NAME=POSIX_TIMESTAMP"
      , help "The name of a timeout parameter in the contract and a value to assign to it (in POSIX milliseconds)."
      ]
    valueArgumentParser = keyValueOption Right integerParser $ mconcat
      [ long "value-arg"
      , metavar "NAME=INTEGER"
      , help "The name of a numeric parameter in the contract and a value to assign to it."
      ]
    integerParser = maybe (Left "Invalid Integer value") Right . readMaybe
    minUTxOParser = option (Lovelace <$> auto) $ mconcat
      [ long "min-utxo"
      , help "An amount which should be used as min ADA requirement for the Contract UTxO."
      , metavar "LOVELACE"
      ]

runCreateCommand :: TxCommand CreateCommand -> CLI ()
runCreateCommand TxCommand { walletAddresses, signingMethod, metadataFile, subCommand=CreateCommand{..}} = case marloweVersion of
  SomeMarloweVersion MarloweV1 -> runCLIExceptT do
    minting' <- case roles of
      Nothing -> pure RoleTokensNone
      Just (MintSimple tokens) -> do
        let toNFT addr = (addr, Left 1)
        pure $ RoleTokensMint $ mkMint $ fmap toNFT <$> tokens
      Just (UseExistingPolicyId policyId) -> pure $ RoleTokensUsePolicy policyId
      Just roles'@(MintConfig _) -> throwE (RolesConfigNotSupportedYet roles')
    ContractId contractId <- run MarloweV1 minting'
    liftIO . print $ A.encode (A.object [("contractId", toJSON . renderTxOutRef $ contractId)])
  where
    readContract :: MarloweVersion v -> ExceptT (CreateCommandError v) CLI (Contract v)
    readContract = \case
      MarloweV1 -> case contractFiles of
        CoreFile filePath -> ExceptT $ liftIO $ first ContractFileDecodingError <$> decodeFileEither filePath
        ExtendedFiles _ _ -> do
          -- extendedContract <- ExceptT $ liftIO $ first (ContractFileDecodingError . Just) <$> decodeFileEither filePath
          throwE ExtendedContractsAreNotSupportedYet

    readMetadata :: ExceptT (CreateCommandError v) CLI TransactionMetadata
    readMetadata = case metadataFile of
      Just filePath -> do
        metadataJSON <- ExceptT $ liftIO $ first (MetadataDecodingFailed . Just) <$> decodeFileEither filePath
        noteT (MetadataDecodingFailed Nothing) $ hoistMaybe (fromJSONEncodedTransactionMetadata metadataJSON)
      Nothing -> pure mempty

    run :: MarloweVersion v -> RoleTokensConfig -> ExceptT (CreateCommandError v) CLI ContractId
    run version rolesDistribution  = do
      contract <- readContract version
      metadata <- readMetadata
      let
        cmd = Create Nothing version walletAddresses rolesDistribution metadata minUTxO contract
      (contractId, transaction) <- ExceptT $ first CreateFailed <$> runTxCommand cmd
      case signingMethod of
        Manual outputFile -> do
          ExceptT $ liftIO $ first TransactionFileWriteFailed <$> C.writeFileTextEnvelope outputFile Nothing transaction
          pure contractId
