{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Web.StandardContract
  where

import Control.Monad.RWS.Strict (MonadIO(liftIO))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
  (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Runtime.Integration.Common (Wallet(..), expectJust)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (InputsApplied(..), WalletAddresses(..))
import Language.Marlowe.Runtime.Web
  (ApplyInputsTxBody, BlockHeader, CreateTxBody, RoleTokenConfig(RoleTokenSimple), TextEnvelope)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (getContract, postContract, putContract)
import Language.Marlowe.Runtime.Web.Common
  ( choose
  , deposit
  , notify
  , signShelleyTransaction'
  , submitContract
  , submitTransaction
  , submitWithdrawal
  , waitUntilConfirmed
  , withdraw
  )
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.Types (RolesConfig(..))
import qualified Plutus.V2.Ledger.Api as PV2
import Servant.Client (ClientM)

data StandardContractInit = StandardContractInit
  { makeInitialDeposit :: ClientM StandardContractFundsDeposited
  , contractCreated :: CreateTxBody
  , createdBlock :: BlockHeader
  }

data StandardContractFundsDeposited  = StandardContractFundsDeposited
  { chooseGimmeTheMoney :: ClientM StandardContractChoiceMade
  , initialFundsDeposited :: ApplyInputsTxBody
  , initialDepositBlock :: BlockHeader
  }

data StandardContractChoiceMade  = StandardContractChoiceMade
  { sendNotify :: ClientM StandardContractNotified
  , gimmeTheMoneyChosen :: ApplyInputsTxBody
  , choiceBlock :: BlockHeader
  }

data StandardContractNotified = StandardContractNotified
  { makeReturnDeposit :: ClientM StandardContractClosed
  , notified :: ApplyInputsTxBody
  , notifiedBlock :: BlockHeader
  }

data StandardContractClosed = StandardContractClosed
  { withdrawPartyAFunds :: ClientM TextEnvelope
  , returnDeposited :: ApplyInputsTxBody
  , returnDepositBlock :: BlockHeader
  }

createStandardContract :: Wallet -> Wallet -> ClientM StandardContractInit
createStandardContract partyAWallet partyBWallet = do
  let partyAWalletAddresses = addresses partyAWallet
  let partyAWebChangeAddress = toDTO $ changeAddress partyAWalletAddresses
  let partyAWebExtraAddresses = Set.map toDTO $ extraAddresses partyAWalletAddresses
  let partyAWebCollataralUtxos = Set.map toDTO $ collateralUtxos partyAWalletAddresses

  let partyBWalletAddresses = addresses partyBWallet
  let partyBWebChangeAddress = toDTO $ changeAddress partyBWalletAddresses
  let partyBWebExtraAddresses = Set.map toDTO $ extraAddresses partyBWalletAddresses
  let partyBWebCollataralUtxos = Set.map toDTO $ collateralUtxos partyBWalletAddresses

  partyBAddress <- expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress $ addresses partyBWallet
  now <- liftIO getCurrentTime
  let (contract, partyA, partyB) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

  Web.CreateTxBody{txBody = createTxBody, ..} <- postContract
    partyAWebChangeAddress
    (Just partyAWebExtraAddresses)
    (Just partyAWebCollataralUtxos)
    Web.PostContractsRequest
      { metadata = mempty
      , version = Web.V1
      , roles = Just $ Mint $ Map.singleton "PartyA" $ RoleTokenSimple partyAWebChangeAddress
      , contract = contract
      , minUTxODeposit = 2_000_000
      , tags = mempty
      }
  createTx <- liftIO $ signShelleyTransaction' createTxBody $ signingKeys partyAWallet
  _ <- putContract contractId createTx
  _ <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId

  pure StandardContractInit
    { createdBlock
    , contractCreated
    , makeInitialDeposit = do
        initialFundsDeposited@InputsApplied{txBody = initialDepositTxBody} <- deposit
          partyAWallet
          contractId
          partyA
          partyA
          ada
          100_000_000
        initialDepositBlock <- submitContract partyAWallet contractId initialDepositTxBody

        pure StandardContractFundsDeposited
          { initialDepositBlock
          , initialFundsDeposited
          , chooseGimmeTheMoney = do
              gimmeTheMoneyChosen@InputsApplied{txBody = choiceTxBody} <- choose
                partyBWallet
                contractId
                "Gimme the money"
                partyB
                0
              choiceBlock <- submit partyBWallet choiceTxBody

              pure StandardContractChoiceMade
                { choiceBlock
                , gimmeTheMoneyChosen
                , sendNotify = do
                    notified@InputsApplied{txBody = notifyTxBody} <- notify partyAWallet contractId
                    notifiedBlock <- submit partyAWallet notifyTxBody

                    pure StandardContractNotified
                      { notifiedBlock
                      , notified
                      , makeReturnDeposit = do
                          returnDeposited@InputsApplied{txBody = returnTxBody} <- deposit
                            partyBWallet
                            contractId
                            partyA
                            partyB
                            ada
                            100_000_000
                          returnDepositBlock <- submit partyBWallet returnTxBody

                          pure StandardContractClosed
                            { returnDepositBlock
                            , returnDeposited
                            , withdrawPartyAFunds = do
                                withdrawTxBody <- withdraw partyAWallet contractId "Party A"
                                (withdrawTxBody,) <$> submitWithdrawal partyAWallet withdrawTxBody
                            }
                      }
                }
          }
    }

standardContract
  :: PV2.Address
  -> UTCTime
  -> NominalDiffTime
  -> (Contract, Party, Party)
standardContract partyBAddress startTime timeoutLength = (contract, partyA, partyB)
  where
    toPosixTime t = PV2.POSIXTime $ floor $ 1000 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t)
    contract = When
      [ Case (Deposit partyA partyA ada (Constant 100_000_000))
          ( When
              [ Case (Choice (ChoiceId "Gimme the money" partyB) [Bound 0 0])
                  ( When
                      [ Case (Notify TrueObs)
                          ( Pay partyA (Party partyB) ada (AvailableMoney partyA ada)
                              ( When
                                  [ Case (Deposit partyA partyB ada (Constant 100_000_000)) Close
                                  ]
                                  (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime))))
                                  Close
                              )
                          )
                      ]
                      (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime)))
                      Close
                  )
              ]
              (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime))
              Close
          )
      ]
      (toPosixTime $ timeoutLength `addUTCTime` startTime)
      Close


    partyA = Role "Party A"
    -- 0x00 = testnet
    partyB = Address (toEnum 0x00) partyBAddress
