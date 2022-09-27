module Language.Marlowe.Runtime.CLI.Command.Tx
  where

import qualified Data.Set as Set
import Language.Marlowe.Runtime.CLI.Option (parseAddress, txOutRefParser)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(WalletAddresses))
import Options.Applicative

data TxCommand cmd = TxCommand
  { walletAddresses :: WalletAddresses
  , signingMethod :: SigningMethod
  , subCommand :: cmd
  }

data SigningMethod
  = Manual

txCommandParser :: Parser cmd -> Parser (TxCommand cmd)
txCommandParser subCommandParser = TxCommand
  <$> walletAddressesParser
  <*> signingMethodParser
  <*> subCommandParser
  where
    walletAddressesParser = WalletAddresses
      <$> changeAddressParser
      <*> extraAddressesParser
      <*> collateralUtxosParser
    signingMethodParser = pure Manual
    changeAddressParser = option (eitherReader parseAddress) $ mconcat
      [ long "change-address"
      , help "The address to which the change of the transaction should be sent."
      , metavar "ADDRESS"
      ]
    extraAddressesParser = fmap Set.fromList $ many $ option (eitherReader parseAddress) $ mconcat
      [ long "address"
      , short 'a'
      , help "An address whose UTXOs can be used as inputs to the transaction"
      , metavar "ADDRESS"
      ]
    collateralUtxosParser = fmap Set.fromList $ many $ option txOutRefParser $ mconcat
      [ long "collateral-utxo"
      , help "An UTXO which may be used for collateral"
      , metavar "UTXO"
      ]
