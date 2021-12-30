-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Contract templates in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Command.Template (
-- * Marlowe CLI Commands
  TemplateCommand(..)
, parseTemplateCommand
, runTemplateCommand
) where


import           Control.Monad.Except                         (MonadIO)
import           Language.Marlowe.CLI.Examples                (makeExample)
import           Language.Marlowe.CLI.Examples.Escrow         (makeEscrowContract)
import           Language.Marlowe.CLI.Examples.Swap           (makeSwapContract)
import           Language.Marlowe.CLI.Examples.Trivial        (makeTrivialContract)
import           Language.Marlowe.CLI.Examples.ZeroCouponBond (makeZeroCouponBond)
import           Language.Marlowe.CLI.Parse                   (parseParty, parseSlot, parseToken)
import           Language.Marlowe.SemanticsTypes              (Party, Token)
import           Plutus.V1.Ledger.Slot                        (Slot)

import qualified Options.Applicative                          as O


-- | Marlowe CLI commands and options for contract templates.
data TemplateCommand =
    -- | Template for trivial contract.
    TemplateTrivial
    {
      bystander          :: Party     -- ^ The party providing the min-ADA.
    , minAda             :: Integer   -- ^ Lovelace in the initial state.
    , minSlot            :: Slot      -- ^ The minimum slot.
    , party              :: Party     -- ^ The party.
    , depositLovelace    :: Integer   -- ^ Lovelace in the deposit.
    , withdrawalLovelace :: Integer   -- ^ Lovelace in the withdrawal.
    , timeout            :: Slot      -- ^ The timeout.
    , contractFile       :: FilePath  -- ^ The output JSON file representing the Marlowe contract.
    , stateFile          :: FilePath  -- ^ The output JSON file representing the Marlowe contract's state.
    }
    -- | Template for escrow contract.
  | TemplateEscrow
    {
      minAda            :: Integer   -- ^ Lovelace in the initial state.
    , price             :: Integer   -- ^ Price of the item for sale, in lovelace.
    , seller            :: Party     -- ^ The seller.
    , buyer             :: Party     -- ^ The buyer.
    , mediator          :: Party     -- ^ The mediator.
    , paymentDeadline   :: Slot      -- ^ The deadline for the buyer to pay.
    , complaintDeadline :: Slot      -- ^ The deadline for the buyer to complain.
    , disputeDeadline   :: Slot      -- ^ The deadline for the seller to dispute a complaint.
    , mediationDeadline :: Slot      -- ^ The deadline for the mediator to decide.
    , contractFile      :: FilePath  -- ^ The output JSON file representing the Marlowe contract.
    , stateFile         :: FilePath  -- ^ The output JSON file representing the Marlowe contract's state.
    }
    -- | Template for swap contract.
  | TemplateSwap
    {
      minAda       :: Integer   -- ^ Lovelace that the first party contributes to the initial state.
    , aParty       :: Party     -- ^ First party.
    , aToken       :: Token     -- ^ First party's token.
    , aAmount      :: Integer   -- ^ Amount of first party's token.
    , aTimeout     :: Slot      -- ^ Timeout for first party's deposit.
    , bParty       :: Party     -- ^ Second party.
    , bToken       :: Token     -- ^ Second party's token.
    , bAmount      :: Integer   -- ^ Amount of second party's token.
    , bTimeout     :: Slot      -- ^ Timeout for second party's deposit.
    , contractFile :: FilePath  -- ^ The output JSON file representing the Marlowe contract.
    , stateFile    :: FilePath  -- ^ The output JSON file representing the Marlowe contract's state.
    }
    -- | Template for zero-coupon bond.
  | TemplateZeroCouponBond
    {
      minAda          :: Integer   -- ^ Lovelace that the lender contributes to the initial state.
    , lender          :: Party     -- ^ The lender.
    , borrower        :: Party     -- ^ The borrower.
    , principal       :: Integer   -- ^ The principal.
    , interest        :: Integer   -- ^ The interest.
    , lendingDeadline :: Slot      -- ^ The lending deadline.
    , paybackDeadline :: Slot      -- ^ The payback deadline.
    , contractFile    :: FilePath  -- ^ The output JSON file representing the Marlowe contract.
    , stateFile       :: FilePath  -- ^ The output JSON file representing the Marlowe contract's state.
    }


-- | Create a contract from a template.
runTemplateCommand :: MonadIO m
                   => TemplateCommand  -- ^ The command.
                   -> m ()             -- ^ Action for runninng the command.
runTemplateCommand TemplateTrivial{..}        = makeExample contractFile stateFile
                                                  $ makeTrivialContract
                                                      bystander
                                                      minAda
                                                      minSlot
                                                      party
                                                      depositLovelace
                                                      withdrawalLovelace
                                                      timeout
runTemplateCommand TemplateEscrow{..}         = makeExample contractFile stateFile
                                                  $ makeEscrowContract
                                                      minAda
                                                      price
                                                      seller
                                                      buyer
                                                      mediator
                                                      paymentDeadline
                                                      complaintDeadline
                                                      disputeDeadline
                                                      mediationDeadline
runTemplateCommand TemplateSwap{..}           = makeExample contractFile stateFile
                                                  $ makeSwapContract
                                                      minAda
                                                      aParty
                                                      aToken
                                                      aAmount
                                                      aTimeout
                                                      bParty
                                                      bToken
                                                      bAmount
                                                      bTimeout
runTemplateCommand TemplateZeroCouponBond{..} = makeExample contractFile stateFile
                                                  $ makeZeroCouponBond
                                                      minAda
                                                      lender
                                                      borrower
                                                      principal
                                                      interest
                                                      lendingDeadline
                                                      paybackDeadline


-- | Parser for template commands.
parseTemplateCommand :: O.Parser TemplateCommand
parseTemplateCommand =
  O.hsubparser
    $ O.commandGroup "Commands for creating Marlowe contracts from templates:"
    <> templateEscrowCommand
    <> templateTrivialCommand
    <> templateSwapCommand
    <> templateZeroCouponBondCommand


-- | Parser for the "simple" command.
templateTrivialCommand :: O.Mod O.CommandFields TemplateCommand
templateTrivialCommand =
  O.command "simple"
    $ O.info templateTrivialOptions
    $ O.progDesc "Create a simple example contract."


-- | Parser for the "trivial" options.
templateTrivialOptions :: O.Parser TemplateCommand
templateTrivialOptions =
  TemplateTrivial
    <$> O.option parseParty (O.long "bystander"           <> O.metavar "PARTY"         <> O.help "The party providing the min-ADA."          )
    <*> O.option O.auto     (O.long "minimum-ada"         <> O.metavar "INTEGER"       <> O.help "Lovelace in the initial state."            )
    <*> O.option parseSlot  (O.long "minimum-slot"        <> O.metavar "SLOT"          <> O.help "The minimum slot."                         )
    <*> O.option parseParty (O.long "party"               <> O.metavar "PARTY"         <> O.help "The party."                                )
    <*> O.option O.auto     (O.long "deposit-lovelace"    <> O.metavar "INTEGER"       <> O.help "Lovelace in the deposit."                  )
    <*> O.option O.auto     (O.long "withdrawal-lovelace" <> O.metavar "INTEGER"       <> O.help "Lovelace in the withdrawal."               )
    <*> O.option parseSlot  (O.long "timeout"             <> O.metavar "SLOT"          <> O.help "The timeout."                              )
    <*> O.strOption         (O.long "out-contract-file"   <> O.metavar "CONTRACT_FILE" <> O.help "JSON output file for the contract."        )
    <*> O.strOption         (O.long "out-state-file"      <> O.metavar "STATE_FILE"    <> O.help "JSON output file for the contract's state.")


-- | Parser for the "escrow" command.
templateEscrowCommand :: O.Mod O.CommandFields TemplateCommand
templateEscrowCommand =
  O.command "escrow"
    $ O.info templateEscrowOptions
    $ O.progDesc "Create an escrow contract."


-- | Parser for the "escrow" options.
templateEscrowOptions :: O.Parser TemplateCommand
templateEscrowOptions =
  TemplateEscrow
    <$> O.option O.auto     (O.long "minimum-ada"        <> O.metavar "INTEGER"       <> O.help "Lovelace in the initial state."                     )
    <*> O.option O.auto     (O.long "price"              <> O.metavar "INTEGER"       <> O.help "The price of the sale, in lovelace."                )
    <*> O.option parseParty (O.long "seller"             <> O.metavar "PARTY"         <> O.help "The seller."                                        )
    <*> O.option parseParty (O.long "buyer"              <> O.metavar "PARTY"         <> O.help "The buyer."                                         )
    <*> O.option parseParty (O.long "mediator"           <> O.metavar "PARTY"         <> O.help "The mediator."                                      )
    <*> O.option parseSlot  (O.long "payment-deadline"   <> O.metavar "SLOT"          <> O.help "The deadline for the buyer to pay."                 )
    <*> O.option parseSlot  (O.long "complaint-deadline" <> O.metavar "SLOT"          <> O.help "The deadline for the buyer to complain."            )
    <*> O.option parseSlot  (O.long "dispute-deadline"   <> O.metavar "SLOT"          <> O.help "The deadline for the seller to dispute a complaint.")
    <*> O.option parseSlot  (O.long "mediation-deadline" <> O.metavar "SLOT"          <> O.help "The deadline for the mediator to decide."           )
    <*> O.strOption         (O.long "out-contract-file"  <> O.metavar "CONTRACT_FILE" <> O.help "JSON output file for the contract."                 )
    <*> O.strOption         (O.long "out-state-file"     <> O.metavar "STATE_FILE"    <> O.help "JSON output file for the contract's state."         )


-- | Parser for the "swap" command.
templateSwapCommand :: O.Mod O.CommandFields TemplateCommand
templateSwapCommand =
  O.command "swap"
    $ O.info templateSwapOptions
    $ O.progDesc "Create a swap contract."


-- | Parser for the "swap" options.
templateSwapOptions :: O.Parser TemplateCommand
templateSwapOptions =
  TemplateSwap
    <$> O.option O.auto     (O.long "minimum-ada"       <> O.metavar "INTEGER"       <> O.help "Lovelace that the first party contributes to the initial state.")
    <*> O.option parseParty (O.long "a-party"           <> O.metavar "PARTY"         <> O.help "The first party."                                               )
    <*> O.option parseToken (O.long "a-token"           <> O.metavar "TOKEN"         <> O.help "The first party's token."                                       )
    <*> O.option O.auto     (O.long "a-amount"          <> O.metavar "INTEGER"       <> O.help "The amount of the first party's token."                         )
    <*> O.option parseSlot  (O.long "a-timeout"         <> O.metavar "SLOT"          <> O.help "The timeout for the first party's deposit."                     )
    <*> O.option parseParty (O.long "b-party"           <> O.metavar "PARTY"         <> O.help "The second party."                                              )
    <*> O.option parseToken (O.long "b-token"           <> O.metavar "TOKEN"         <> O.help "The second party's token."                                      )
    <*> O.option O.auto     (O.long "b-amount"          <> O.metavar "INTEGER"       <> O.help "The amount of the second party's token."                        )
    <*> O.option parseSlot  (O.long "b-timeout"         <> O.metavar "SLOT"          <> O.help "The timeout for the second party's deposit."                    )
    <*> O.strOption         (O.long "out-contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON output file for the contract."                             )
    <*> O.strOption         (O.long "out-state-file"    <> O.metavar "STATE_FILE"    <> O.help "JSON output file for the contract's state."                     )


-- | Parser for the "zcb" command.
templateZeroCouponBondCommand :: O.Mod O.CommandFields TemplateCommand
templateZeroCouponBondCommand =
  O.command "zcb"
    $ O.info templateZeroCouponBondOptions
    $ O.progDesc "Create a zero-coupon bond."


-- | Parser for the "zcb" options.
templateZeroCouponBondOptions :: O.Parser TemplateCommand
templateZeroCouponBondOptions =
  TemplateZeroCouponBond
    <$> O.option O.auto     (O.long "minimum-ada"        <> O.metavar "INTEGER"       <> O.help "Lovelace that the lender contributes to the initial state.")
    <*> O.option parseParty (O.long "lender"             <> O.metavar "PARTY"         <> O.help "The lender."                                               )
    <*> O.option parseParty (O.long "borrower"           <> O.metavar "PARTY"         <> O.help "The borrower."                                             )
    <*> O.option O.auto     (O.long "principal"          <> O.metavar "INTEGER"       <> O.help "The principal, in lovelace."                               )
    <*> O.option O.auto     (O.long "interest"           <> O.metavar "INTEGER"       <> O.help "The interest, in lovelace."                                )
    <*> O.option parseSlot  (O.long "lending-deadline"   <> O.metavar "SLOT"          <> O.help "The lending deadline."                                     )
    <*> O.option parseSlot  (O.long "repayment-deadline" <> O.metavar "SLOT"          <> O.help "The repayment deadline."                                   )
    <*> O.strOption         (O.long "out-contract-file"  <> O.metavar "CONTRACT_FILE" <> O.help "JSON output file for the contract."                        )
    <*> O.strOption         (O.long "out-state-file"     <> O.metavar "STATE_FILE"    <> O.help "JSON output file for the contract's state."                )
