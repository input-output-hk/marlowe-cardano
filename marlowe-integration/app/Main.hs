{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main
  where

import Cardano.Api (AsType(..), ShelleyWitnessSigningKey(..), deserialiseFromTextEnvelope, signShelleyTransaction)
import Data.Aeson (decodeFileStrict)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (Address, fromBech32, toBech32)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), InputsApplied(..), MarloweTxCommand(..), RoleTokensConfig(..), WalletAddresses(..))
import Network.Protocol.Job.Client (liftCommand, liftCommandWait)
import Test.Integration.Marlowe
import qualified Test.Integration.Marlowe as M (LocalTestnet(..))

main :: IO ()
main = withLocalMarloweRuntime \MarloweRuntime{..} -> do
  putStr "Workspace: "
  putStrLn $ workspaceDir $ M.workspace testnet

  (address, signingKey) <- getFirstWallet testnet
  putStr "Loaded wallet: "
  print $ fromJust $ toBech32 address

  let walletAddresses = WalletAddresses address mempty mempty

  createContractResult <- runTxJobClient $ liftCommand $ Create
    Nothing
    MarloweV1
    walletAddresses
    RoleTokensNone
    mempty
    2_000_000
    V1.Close

  ContractCreated{..} <- case createContractResult of
    Left err -> fail $ show err
    Right c -> pure c

  print ContractCreated{..}

  let createTx = signShelleyTransaction txBody [signingKey]

  submitCreateContractResult <- runTxJobClient $ liftCommandWait $ Submit createTx

  createdBlock <- case submitCreateContractResult of
    Left err -> fail $ show err
    Right b -> pure b

  putStr "Created block: "
  print createdBlock

  closeResult <- runTxJobClient $ liftCommand $ ApplyInputs
    MarloweV1
    walletAddresses
    contractId
    mempty
    Nothing
    Nothing
    []

  InputsApplied{..} <- case closeResult of
    Left err -> fail $ show err
    Right a -> pure a

  print InputsApplied{..}

  let closeTx = signShelleyTransaction txBody [signingKey]

  submitCloseResult <- runTxJobClient $ liftCommandWait $ Submit closeTx

  closedBlock <- case submitCloseResult of
    Left err -> fail $ show err
    Right b -> pure b

  putStr "Closed block: "
  print closedBlock

getFirstWallet :: LocalTestnet -> IO (Address, ShelleyWitnessSigningKey)
getFirstWallet LocalTestnet{..} = do
  let PaymentKeyPair{..} = head wallets
  address <- fromJust . fromBech32 . T.pack <$> execCli
    [ "address", "build"
    , "--verification-key-file", paymentVKey
    , "--testnet-magic", "1"
    ]
  textEnvelope <- fromJust <$> decodeFileStrict paymentSKey
  pure
    ( address
    , WitnessGenesisUTxOKey
        $ fromRight (error "Failed to decode text envelope")
        $ deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope
    )
