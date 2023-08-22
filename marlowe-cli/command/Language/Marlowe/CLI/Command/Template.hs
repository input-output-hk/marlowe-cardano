{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Contract templates in the Marlowe CLI tool.
module Language.Marlowe.CLI.Command.Template (
  -- * Marlowe CLI Commands
  OutputFiles (..),
  TemplateCommand (..),
  parseTemplateCommand,
  parseTemplateCommandOutputFiles,
  runTemplateCommand,
) where

import Actus.Marlowe (defaultRiskFactors, genContract')
import Control.Monad.Except (MonadError, MonadIO)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Command.Parse (
  parseParty,
  parseTimeout,
  parseToken,
  readByteStringEither,
  readPartyEither,
  readTokenName,
  timeoutHelpMsg,
 )
import Language.Marlowe.CLI.Examples (makeExample)
import Language.Marlowe.CLI.IO (decodeFileStrict)
import Language.Marlowe.CLI.Types (CliError (..), SomeTimeout, toMarloweExtendedTimeout, toMarloweTimeout)
import Language.Marlowe.Extended.V1 as E (Contract (..), Party, Token, Value (..))
import Language.Marlowe.Util (ada)
import Marlowe.Contracts (coveredCall, escrow, swap, trivial, zeroCouponBond)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.List.Split qualified as List
import Data.Map.Append.Strict (AppendMap (..))
import Language.Marlowe qualified as M
import Language.Marlowe.CLI.Test.CLI.Interpret (initialMarloweState)
import Language.Marlowe.CLI.Test.Contract.Source (makeContract)
import Marlowe.Contracts.ChunkedValueTransfer (chunkedValueTransfer)
import Marlowe.Contracts.ChunkedValueTransfer qualified as ChunkedValueTransfer
import Marlowe.Contracts.Raffle (raffle)
import Marlowe.Contracts.Raffle qualified as Raffle
import Options.Applicative qualified as O
import Plutus.V1.Ledger.Value qualified as P
import Text.Read (readEither)

-- | Marlowe CLI commands and options for contract templates.
data TemplateCommand
  = -- | Template for trivial contract.
    TemplateTrivial
      { bystander :: Party
      -- ^ The party providing the min-ADA.
      , minAda :: Integer
      -- ^ Lovelace in the initial state.
      , party :: Party
      -- ^ The party.
      , depositLovelace :: Integer
      -- ^ Lovelace in the deposit.
      , withdrawalLovelace :: Integer
      -- ^ Lovelace in the withdrawal.
      , timeout :: SomeTimeout
      -- ^ The timeout.
      }
  | -- | Template for escrow contract.
    TemplateEscrow
      { minAda :: Integer
      -- ^ Lovelace in the initial state.
      , price :: Integer
      -- ^ Price of the item for sale, in lovelace.
      , seller :: Party
      -- ^ The seller.
      , buyer :: Party
      -- ^ The buyer.
      , mediator :: Party
      -- ^ The mediator.
      , paymentDeadline :: SomeTimeout
      -- ^ The deadline for the buyer to pay.
      , complaintDeadline :: SomeTimeout
      -- ^ The deadline for the buyer to complain.
      , disputeDeadline :: SomeTimeout
      -- ^ The deadline for the seller to dispute a complaint.
      , mediationDeadline :: SomeTimeout
      -- ^ The deadline for the mediator to decide.
      }
  | -- | Template for swap contract.
    TemplateSwap
      { minAda :: Integer
      -- ^ Lovelace that the first party contributes to the initial state.
      , aParty :: Party
      -- ^ First party.
      , aToken :: Token
      -- ^ First party's token.
      , aAmount :: Integer
      -- ^ Amount of first party's token.
      , aTimeout :: SomeTimeout
      -- ^ Timeout for first party's deposit.
      , bParty :: Party
      -- ^ Second party.
      , bToken :: Token
      -- ^ Second party's token.
      , bAmount :: Integer
      -- ^ Amount of second party's token.
      , bTimeout :: SomeTimeout
      -- ^ Timeout for second party's deposit.
      }
  | -- | Template for zero-coupon bond.
    TemplateZeroCouponBond
      { minAda :: Integer
      -- ^ Lovelace that the lender contributes to the initial state.
      , lender :: Party
      -- ^ The lender.
      , borrower :: Party
      -- ^ The borrower.
      , principal :: Integer
      -- ^ The principal.
      , interest :: Integer
      -- ^ The interest.
      , lendingDeadline :: SomeTimeout
      -- ^ The lending deadline.
      , paybackDeadline :: SomeTimeout
      -- ^ The payback deadline.
      }
  | -- | Template for covered call.
    TemplateCoveredCall
      { minAda :: Integer
      -- ^ Lovelace that the lender contributes to the initial state.
      , issuer :: Party
      -- ^ The issuer.
      , counterparty :: Party
      -- ^ The counter-party.
      , currency :: Token
      -- ^ The currency token.
      , underlying :: Token
      -- ^ The underlying token.
      , strike :: Integer
      -- ^ The strike in currency.
      , amount :: Integer
      -- ^ The amount of underlying.
      , issueDate :: SomeTimeout
      -- ^ The issue date.
      , maturityDate :: SomeTimeout
      -- ^ The maturity date.
      , settlementDate :: SomeTimeout
      -- ^ The settlement date.
      }
  | -- | Template for actus contracts.
    TemplateActus
      { minAda :: Integer
      -- ^ Lovelace that the lender contributes to the initial state.
      , party :: Party
      -- ^ The party.
      , counterparty :: Party
      -- ^ The counter-party.
      , actusTermsFile :: FilePath
      -- ^ The Actus contract terms.
      }
  | TemplateRaffle
      { minAda :: Integer
      , sponsor :: Raffle.Sponsor
      , oracle :: Raffle.Oracle
      , chunkSize :: Raffle.ChunkSize
      , parties :: NonEmpty Party
      , prizeNFTPerRound :: NonEmpty Token
      , depositDeadline :: SomeTimeout
      , selectDeadline :: SomeTimeout
      , payoutDeadline :: SomeTimeout
      }
  | TemplateChunkedValueTransfer
      { minAda :: Integer
      , sender :: ChunkedValueTransfer.Sender
      , recipientsAmounts :: ChunkedValueTransfer.RecipientsAmounts
      , payoutChunkSize :: ChunkedValueTransfer.PayoutChunkSize
      , timeout :: SomeTimeout
      }
  deriving stock (Eq, Generic, Show)

-- | Paths for Output Files for Template Contracts
data OutputFiles = OutputFiles
  { contractFile :: FilePath
  -- ^ The output JSON file representing the Marlowe contract.
  , stateFile :: FilePath
  -- ^ The output JSON file representing the Marlowe contract's state.
  }

-- | Create a contract from a template.
runTemplateCommand
  :: (MonadError CliError m)
  => (MonadIO m)
  => TemplateCommand
  -- ^ The command.
  -> OutputFiles
  -- ^ FilePath's for contractFile and stateFile
  -> m ()
  -- ^ Action for runninng the command.
runTemplateCommand TemplateTrivial{..} OutputFiles{..} = do
  timeout' <- toMarloweExtendedTimeout timeout
  marloweContract <-
    makeContract $
      trivial
        party
        depositLovelace
        withdrawalLovelace
        timeout'
  let marloweState = initialMarloweState bystander minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateEscrow{..} OutputFiles{..} = do
  paymentDeadline' <- toMarloweExtendedTimeout paymentDeadline
  complaintDeadline' <- toMarloweExtendedTimeout complaintDeadline
  disputeDeadline' <- toMarloweExtendedTimeout disputeDeadline
  mediationDeadline' <- toMarloweExtendedTimeout mediationDeadline
  marloweContract <-
    makeContract $
      escrow
        (Constant price)
        seller
        buyer
        mediator
        paymentDeadline'
        complaintDeadline'
        disputeDeadline'
        mediationDeadline'
  let marloweState = initialMarloweState mediator minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateSwap{..} OutputFiles{..} = do
  aTimeout' <- toMarloweExtendedTimeout aTimeout
  bTimeout' <- toMarloweExtendedTimeout bTimeout
  marloweContract <-
    makeContract $
      swap
        aParty
        aToken
        (Constant aAmount)
        aTimeout'
        bParty
        bToken
        (Constant bAmount)
        bTimeout'
        Close
  let marloweState = initialMarloweState aParty minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateZeroCouponBond{..} OutputFiles{..} = do
  lendingDeadline' <- toMarloweExtendedTimeout lendingDeadline
  paybackDeadline' <- toMarloweExtendedTimeout paybackDeadline
  marloweContract <-
    makeContract $
      zeroCouponBond
        lender
        borrower
        lendingDeadline'
        paybackDeadline'
        (Constant principal)
        (Constant principal `AddValue` Constant interest)
        ada
        Close
  let marloweState = initialMarloweState lender minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateCoveredCall{..} OutputFiles{..} = do
  issueDate' <- toMarloweExtendedTimeout issueDate
  maturityDate' <- toMarloweExtendedTimeout maturityDate
  settlementDate' <- toMarloweExtendedTimeout settlementDate
  marloweContract <-
    makeContract $
      coveredCall
        issuer
        counterparty
        Nothing
        currency
        underlying
        (Constant strike)
        (Constant amount)
        issueDate'
        maturityDate'
        settlementDate'
  let marloweState = initialMarloweState issuer minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateActus{..} OutputFiles{..} = do
  ct <- decodeFileStrict actusTermsFile
  marloweContract <- makeContract $ genContract' (party, counterparty) defaultRiskFactors ct
  let marloweState = initialMarloweState party minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateRaffle{..} OutputFiles{..} = do
  depositDeadline' <- toMarloweTimeout depositDeadline
  selectDeadline' <- toMarloweTimeout selectDeadline
  payoutDeadline' <- toMarloweTimeout payoutDeadline
  let marloweContract =
        raffle
          sponsor
          oracle
          chunkSize
          parties
          prizeNFTPerRound
          depositDeadline'
          selectDeadline'
          payoutDeadline'
  let Raffle.Sponsor sponsorParty = sponsor
      marloweState = initialMarloweState sponsorParty minAda
  makeExample contractFile stateFile (marloweContract, marloweState)
runTemplateCommand TemplateChunkedValueTransfer{..} OutputFiles{..} = do
  timeout' <- toMarloweTimeout timeout
  let ChunkedValueTransfer.Sender senderParty = sender
      marloweState = initialMarloweState senderParty minAda
      marloweContract =
        chunkedValueTransfer
          sender
          recipientsAmounts
          payoutChunkSize
          timeout'
  makeExample contractFile stateFile (marloweContract, marloweState)

-- | Parser for template commands.
parseTemplateCommand :: O.Parser TemplateCommand
parseTemplateCommand =
  O.hsubparser $
    O.commandGroup "Commands for creating Marlowe contracts from templates:"
      <> templateEscrowCommand
      <> templateTrivialCommand
      <> templateSwapCommand
      <> templateZeroCouponBondCommand
      <> templateCoveredCallCommand
      <> templateActusCommand
      <> templateRaffleCommand
      <> templateChunkedValueTransferCommand

parseTemplateCommandOutputFiles :: O.Parser OutputFiles
parseTemplateCommandOutputFiles =
  OutputFiles
    <$> O.strOption
      ( O.long "out-contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON output file for the contract."
      )
    <*> O.strOption
      ( O.long "out-state-file" <> O.metavar "STATE_FILE" <> O.help "JSON output file for the contract's state."
      )

-- | Parser for the "simple" command.
templateTrivialCommand :: O.Mod O.CommandFields TemplateCommand
templateTrivialCommand =
  O.command "simple" $
    O.info templateTrivialOptions $
      O.progDesc "Create a simple example contract."

-- | Parser for the "trivial" options.
templateTrivialOptions :: O.Parser TemplateCommand
templateTrivialOptions =
  TemplateTrivial
    <$> O.option
      parseParty
      (O.long "bystander" <> O.metavar "PARTY" <> O.help "The party providing the min-ADA.")
    <*> O.option
      O.auto
      (O.long "minimum-ada" <> O.metavar "INTEGER" <> O.help "Lovelace in the initial state.")
    <*> O.option
      parseParty
      (O.long "party" <> O.metavar "PARTY" <> O.help "The party.")
    <*> O.option
      O.auto
      (O.long "deposit-lovelace" <> O.metavar "INTEGER" <> O.help "Lovelace in the deposit.")
    <*> O.option
      O.auto
      (O.long "withdrawal-lovelace" <> O.metavar "INTEGER" <> O.help "Lovelace in the withdrawal.")
    <*> O.option
      parseTimeout
      (O.long "timeout" <> O.metavar "TIMEOUT" <> O.help ("The timeout. " <> timeoutHelpMsg))

-- | Parser for the "escrow" command.
templateEscrowCommand :: O.Mod O.CommandFields TemplateCommand
templateEscrowCommand =
  O.command "escrow" $
    O.info templateEscrowOptions $
      O.progDesc "Create an escrow contract."

-- | Parser for the "escrow" options.
templateEscrowOptions :: O.Parser TemplateCommand
templateEscrowOptions =
  TemplateEscrow
    <$> O.option
      O.auto
      ( O.long "minimum-ada" <> O.metavar "INTEGER" <> O.help "Lovelace in the initial state."
      )
    <*> O.option
      O.auto
      ( O.long "price" <> O.metavar "INTEGER" <> O.help "The price of the sale, in lovelace."
      )
    <*> O.option
      parseParty
      ( O.long "seller" <> O.metavar "PARTY" <> O.help "The seller."
      )
    <*> O.option
      parseParty
      ( O.long "buyer" <> O.metavar "PARTY" <> O.help "The buyer."
      )
    <*> O.option
      parseParty
      ( O.long "mediator" <> O.metavar "PARTY" <> O.help "The mediator."
      )
    <*> O.option
      parseTimeout
      ( O.long "payment-deadline"
          <> O.metavar "TIMEOUT"
          <> O.help ("The deadline for the buyer to pay. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "complaint-deadline"
          <> O.metavar "TIMEOUT"
          <> O.help ("The deadline for the buyer to complain. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "dispute-deadline"
          <> O.metavar "TIMEOUT"
          <> O.help ("The deadline for the seller to dispute a complaint. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "mediation-deadline"
          <> O.metavar "TIMEOUT"
          <> O.help ("The deadline for the mediator to decide. " <> timeoutHelpMsg)
      )

-- | Parser for the "swap" command.
templateSwapCommand :: O.Mod O.CommandFields TemplateCommand
templateSwapCommand =
  O.command "swap" $
    O.info templateSwapOptions $
      O.progDesc "Create a swap contract."

-- | Parser for the "swap" options.
templateSwapOptions :: O.Parser TemplateCommand
templateSwapOptions =
  TemplateSwap
    <$> O.option
      O.auto
      ( O.long "minimum-ada"
          <> O.metavar "INTEGER"
          <> O.help "Lovelace that the first party contributes to the initial state."
      )
    <*> O.option
      parseParty
      ( O.long "a-party" <> O.metavar "PARTY" <> O.help "The first party."
      )
    <*> O.option
      parseToken
      ( O.long "a-token" <> O.metavar "TOKEN" <> O.help "The first party's token."
      )
    <*> O.option
      O.auto
      ( O.long "a-amount" <> O.metavar "INTEGER" <> O.help "The amount of the first party's token."
      )
    <*> O.option
      parseTimeout
      ( O.long "a-timeout"
          <> O.metavar "TIMEOUT"
          <> O.help ("The timeout for the first party's deposit. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseParty
      ( O.long "b-party" <> O.metavar "PARTY" <> O.help "The second party."
      )
    <*> O.option
      parseToken
      ( O.long "b-token" <> O.metavar "TOKEN" <> O.help "The second party's token."
      )
    <*> O.option
      O.auto
      ( O.long "b-amount" <> O.metavar "INTEGER" <> O.help "The amount of the second party's token."
      )
    <*> O.option
      parseTimeout
      ( O.long "b-timeout"
          <> O.metavar "TIMEOUT"
          <> O.help ("The timeout for the second party's deposit. " <> timeoutHelpMsg)
      )

-- | Parser for the "zcb" command.
templateZeroCouponBondCommand :: O.Mod O.CommandFields TemplateCommand
templateZeroCouponBondCommand =
  O.command "zcb" $
    O.info templateZeroCouponBondOptions $
      O.progDesc "Create a zero-coupon bond."

-- | Parser for the "zcb" options.
templateZeroCouponBondOptions :: O.Parser TemplateCommand
templateZeroCouponBondOptions =
  TemplateZeroCouponBond
    <$> O.option
      O.auto
      ( O.long "minimum-ada"
          <> O.metavar "INTEGER"
          <> O.help "Lovelace that the lender contributes to the initial state."
      )
    <*> O.option
      parseParty
      ( O.long "lender" <> O.metavar "PARTY" <> O.help "The lender."
      )
    <*> O.option
      parseParty
      ( O.long "borrower" <> O.metavar "PARTY" <> O.help "The borrower."
      )
    <*> O.option
      O.auto
      ( O.long "principal" <> O.metavar "INTEGER" <> O.help "The principal, in lovelace."
      )
    <*> O.option
      O.auto
      ( O.long "interest" <> O.metavar "INTEGER" <> O.help "The interest, in lovelace."
      )
    <*> O.option
      parseTimeout
      ( O.long "lending-deadline" <> O.metavar "TIMEOUT" <> O.help ("The lending deadline. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "repayment-deadline" <> O.metavar "TIMEOUT" <> O.help ("The repayment deadline. " <> timeoutHelpMsg)
      )

-- | Parser for the "coveredCall" command.
templateCoveredCallCommand :: O.Mod O.CommandFields TemplateCommand
templateCoveredCallCommand =
  O.command "coveredCall" $
    O.info templateCoveredCallOptions $
      O.progDesc "Create a covered call Option."

-- | Parser for the "coveredCall" options.
templateCoveredCallOptions :: O.Parser TemplateCommand
templateCoveredCallOptions =
  TemplateCoveredCall
    <$> O.option
      O.auto
      ( O.long "minimum-ada"
          <> O.metavar "INTEGER"
          <> O.help "Lovelace that the lender contributes to the initial state."
      )
    <*> O.option
      parseParty
      ( O.long "issuer" <> O.metavar "PARTY" <> O.help "The issuer."
      )
    <*> O.option
      parseParty
      ( O.long "counter-party" <> O.metavar "PARTY" <> O.help "The counter-party."
      )
    <*> O.option
      parseToken
      ( O.long "currency" <> O.metavar "TOKEN" <> O.help "The curreny."
      )
    <*> O.option
      parseToken
      ( O.long "underlying" <> O.metavar "TOKEN" <> O.help "The underlying asset."
      )
    <*> O.option
      O.auto
      ( O.long "strike" <> O.metavar "INTEGER" <> O.help "The strike, in currency."
      )
    <*> O.option
      O.auto
      ( O.long "amount" <> O.metavar "INTEGER" <> O.help "The amount of underlying"
      )
    <*> O.option
      parseTimeout
      ( O.long "issue-date" <> O.metavar "TIMEOUT" <> O.help ("The issue date. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "maturity-date" <> O.metavar "TIMEOUT" <> O.help ("The maturity date. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "settlement-date" <> O.metavar "TIMEOUT" <> O.help ("The settlement date. " <> timeoutHelpMsg)
      )

-- | Parser for the "actus" command.
templateActusCommand :: O.Mod O.CommandFields TemplateCommand
templateActusCommand =
  O.command "actus" $
    O.info templateActusOptions $
      O.progDesc "Create an Actus contract."

-- | Parser for the "actus" options.
templateActusOptions :: O.Parser TemplateCommand
templateActusOptions =
  TemplateActus
    <$> O.option
      O.auto
      ( O.long "minimum-ada"
          <> O.metavar "INTEGER"
          <> O.help "Lovelace that the party contributes to the initial state."
      )
    <*> O.option
      parseParty
      ( O.long "party" <> O.metavar "PARTY" <> O.help "The party."
      )
    <*> O.option
      parseParty
      ( O.long "counter-party" <> O.metavar "PARTY" <> O.help "The counterparty."
      )
    <*> O.strOption
      ( O.long "actus-terms-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the actus contract terms."
      )

templateRaffleCommand :: O.Mod O.CommandFields TemplateCommand
templateRaffleCommand =
  O.command "raffle" $
    O.info templateRaffleOptions $
      O.progDesc "Create a Raffle contract."

templateRaffleOptions :: O.Parser TemplateCommand
templateRaffleOptions =
  TemplateRaffle
    <$> O.option
      O.auto
      ( O.long "minimum-ada"
          <> O.metavar "INTEGER"
          <> O.help "Lovelace that the party contributes to the initial state."
      )
    <*> O.option
      (Raffle.Sponsor <$> parseParty)
      ( O.long "sponsor" <> O.metavar "PARTY" <> O.help "The sponsor."
      )
    <*> O.option
      (Raffle.Oracle <$> parseParty)
      ( O.long "oracle" <> O.metavar "PARTY" <> O.help "The oracle."
      )
    <*> O.option
      (Raffle.ChunkSize <$> O.auto)
      ( O.long "chunk-size" <> O.metavar "INTEGER" <> O.help "The chunk size."
      )
    <*> (NEL.fromList <$> O.many (O.option parseParty (O.long "parties" <> O.metavar "PARTIES" <> O.help "The parties.")))
    <*> (NEL.fromList <$> O.many (O.option parseToken (O.long "prizes" <> O.metavar "PRIZES" <> O.help "The prizes.")))
    <*> O.option
      parseTimeout
      ( O.long "deposit-deadline" <> O.metavar "TIMEOUT" <> O.help ("The deposit deadline. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "select-deadline" <> O.metavar "TIMEOUT" <> O.help ("The select deadline. " <> timeoutHelpMsg)
      )
    <*> O.option
      parseTimeout
      ( O.long "payout-deadline" <> O.metavar "TIMEOUT" <> O.help ("The payout deadline. " <> timeoutHelpMsg)
      )

templateChunkedValueTransferCommand :: O.Mod O.CommandFields TemplateCommand
templateChunkedValueTransferCommand =
  O.command "chunked-value-transfer" $
    O.info templateChunkedValueTransferOptions $
      O.progDesc
        "Create a contract \"trivial\" Marlowe contract which transfers set of tokens from the sender to the recipient in chunks."

templateChunkedValueTransferOptions :: O.Parser TemplateCommand
templateChunkedValueTransferOptions = do
  let parsePartyAsset :: O.ReadM (Party, Token, Integer)
      parsePartyAsset = O.eitherReader $ \s ->
        case List.splitOn "," s of
          [party, currencySymbol, token, amount] -> do
            party' <- readPartyEither party
            (amount' :: Integer) <- readEither amount
            currencySymbol' <- readByteStringEither currencySymbol
            pure (party', M.Token (M.CurrencySymbol currencySymbol') (readTokenName token), amount')
          _ -> Left "Invalid deposit format. Expecting: CURRENCY_SYMBOL,TOKEN,AMOUNT"

      foldRecipientsAmounts =
        ChunkedValueTransfer.RecipientsAmounts . unAppendMap . foldMap \(party, M.Token currencySymbol tokenName, amount) -> do
          let value = P.singleton currencySymbol tokenName amount
          AppendMap $ Map.singleton (ChunkedValueTransfer.Recipient party) value

      recipientsAmountsOption =
        foldRecipientsAmounts
          <$> O.some
            ( O.option
                parsePartyAsset
                ( O.long "recipient-asset"
                    <> O.metavar "RECIPIENT_ASSET"
                    <> O.help "The recipient asset in a format \"party,currency symbol,token name, amount\"."
                )
            )

  TemplateChunkedValueTransfer
    <$> O.option
      O.auto
      ( O.long "minimum-ada"
          <> O.metavar "INTEGER"
          <> O.help "Lovelace that the party contributes to the initial state."
      )
    <*> O.option
      (ChunkedValueTransfer.Sender <$> parseParty)
      (O.long "sender" <> O.metavar "PARTY" <> O.help "The sender.")
    <*> recipientsAmountsOption
    <*> O.option
      (ChunkedValueTransfer.PayoutChunkSize <$> O.auto)
      (O.long "payout-chunk-size" <> O.metavar "PAYOUT-CHUNK-SIZE" <> O.help "The payout chunk size.")
    <*> O.option
      parseTimeout
      (O.long "timeout" <> O.metavar "TIMEOUT" <> O.help ("The timeout. " <> timeoutHelpMsg))
