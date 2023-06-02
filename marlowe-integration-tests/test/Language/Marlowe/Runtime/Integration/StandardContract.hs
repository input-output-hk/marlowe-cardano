{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Integration.StandardContract
  where

import Cardano.Api (BabbageEra, TxBody)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Client (createContract)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId
  , MarloweMetadata(..)
  , MarloweMetadataTag
  , MarloweTransactionMetadata(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , emptyMarloweTransactionMetadata
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.Integration.Common
  ( Integration
  , Wallet(..)
  , choose
  , contractCreatedToContractHeader
  , deposit
  , expectJust
  , expectRight
  , notify
  , submit
  , withdraw
  )
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), InputsApplied(..), RoleTokensConfig(..), WalletAddresses(changeAddress), mkMint)
import qualified Plutus.V2.Ledger.Api as PV2

data StandardContractInit v = StandardContractInit
  { makeInitialDeposit :: Integration (StandardContractFundsDeposited v)
  , contractCreated :: ContractCreated BabbageEra v
  , createdBlock :: BlockHeader
  }

standardContractHeader :: StandardContractInit v -> ContractHeader
standardContractHeader StandardContractInit{..} = contractCreatedToContractHeader createdBlock contractCreated

standardContractId :: StandardContractInit v -> ContractId
standardContractId StandardContractInit{contractCreated=ContractCreated{..}} = contractId

data StandardContractFundsDeposited v = StandardContractFundsDeposited
  { chooseGimmeTheMoney :: Integration (StandardContractChoiceMade v)
  , initialFundsDeposited :: InputsApplied BabbageEra v
  , initialDepositBlock :: BlockHeader
  }

data StandardContractChoiceMade v = StandardContractChoiceMade
  { sendNotify :: Integration (StandardContractNotified v)
  , gimmeTheMoneyChosen :: InputsApplied BabbageEra v
  , choiceBlock :: BlockHeader
  }

data StandardContractNotified v = StandardContractNotified
  { makeReturnDeposit :: Integration (StandardContractClosed v)
  , notified :: InputsApplied BabbageEra v
  , notifiedBlock :: BlockHeader
  }

data StandardContractClosed v = StandardContractClosed
  { withdrawPartyAFunds :: Integration (TxBody BabbageEra, BlockHeader)
  , returnDeposited :: InputsApplied BabbageEra v
  , returnDepositBlock :: BlockHeader
  }

createStandardContract :: Wallet -> Wallet -> Integration (StandardContractInit 'V1)
createStandardContract = createStandardContractWithTags mempty

createStandardContractWithTags :: Set MarloweMetadataTag -> Wallet -> Wallet -> Integration (StandardContractInit 'V1)
createStandardContractWithTags tags partyAWallet partyBWallet = do
  partyBAddress <- expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress $ addresses partyBWallet
  now <- liftIO getCurrentTime
  let (contract, partyA, partyB) = standardContract partyBAddress now $ secondsToNominalDiffTime 100
  result <- createContract
    Nothing
    MarloweV1
    (addresses partyAWallet)
    (RoleTokensMint $ mkMint $ pure ("Party A", (changeAddress $ addresses partyAWallet, Nothing)))
    (if Set.null tags
        then emptyMarloweTransactionMetadata
        else emptyMarloweTransactionMetadata
          { marloweMetadata = Just MarloweMetadata
            { tags = Map.fromSet (const Nothing) tags
            , continuations = Nothing
            }
          }
    )
    2_000_000
    contract
  contractCreated@ContractCreated{contractId, txBody = createTxBody} <- expectRight "failed to create standard contract" result
  createdBlock <- submit partyAWallet createTxBody

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
        initialDepositBlock <- submit partyAWallet initialDepositTxBody

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
                                (withdrawTxBody,) <$> submit partyAWallet withdrawTxBody
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
