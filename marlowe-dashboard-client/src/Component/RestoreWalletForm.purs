module Componenet.RestoreWalletForm where

import Prologue

import API.Marlowe.Run.Wallet.CentralizedTestnet (RestoreError(..))
import Capability.Marlowe (class ManageMarlowe, restoreWallet)
import Component.Button.Types as Button
import Component.Button.View (button)
import Component.Contacts.Types (WalletDetails)
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.Either (isLeft)
import Data.Filterable (filter)
import Data.MnemonicPhrase (class CheckMnemonic, MnemonicPhrase)
import Data.Set (Set)
import Data.Tuple (uncurry)
import Data.WalletNickname (WalletNickname)
import Effect.Aff.Class (class MonadAff)
import Forms (AsyncInput(..), MnemonicPhraseInput)
import Forms as Forms
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (useForm)
import Halogen.Form as Form
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))

type Input = Set WalletNickname

type RestoreWalletInput = Tuple String MnemonicPhraseInput

type RestoreWalletOutput = Tuple WalletNickname MnemonicPhrase

data Msg
  = Closed
  | Restored WalletNickname WalletDetails

type Component q m =
  H.Component q Input Msg m

component
  :: forall q m
   . MonadAff m
  => CheckMnemonic m
  => ManageMarlowe m
  => Component q m
component = Hooks.component \{ outputToken } input -> Hooks.do
  form <- Hooks.captures { input } Hooks.useMemo \_ ->
    Form.split (Forms.walletNickname input) Forms.mnemonicPhrase
  { result, html } <- useForm form $ Tuple "" $ AsyncInput "" NotAsked
  Tuple canRestore canRestoreId <- Hooks.useState true
  Tuple serverError serverErrorId <- Hooks.useState ""
  let cancel = Hooks.raise outputToken Closed
  let
    submit nickname mnemonic = do
      Hooks.put serverErrorId ""
      Hooks.put canRestoreId false
      response <- lift $ restoreWallet nickname mnemonic ""
      Hooks.put canRestoreId $ isLeft response
      case response of
        Left InvalidMnemonic ->
          Hooks.put serverErrorId "Invalid mnemonic phrase."
        Left _ ->
          Hooks.put serverErrorId "Error from server."
        Right walletDetails ->
          Hooks.raise outputToken $ Restored nickname walletDetails
  Hooks.pure do
    HH.div [ classNames [ "p-5", "lg:p-6", "space-y-2" ] ]
      [ HH.h2
          [ classNames [ "font-bold" ] ]
          [ HH.text "Restore testnet wallet" ]
      , html [ "relative", "space-y-4" ]
      , HH.p_
          [ HH.b_ [ HH.text "IMPORTANT:" ]
          -- FIXME: as part of SCP-3173, Write a section in the Marlowe Run documentation and add a link to it
          , HH.text "Do not use a real wallet phrase <read more>"
          ]
      -- TODO replace with progress buttons when refactored.
      , HH.p [ classNames Css.inputError ] [ HH.text serverError ]
      , HH.div
          [ classNames [ "flex", "gap-4" ] ]
          [ button
              Button.Secondary
              (Just cancel)
              []
              [ HH.text "Cancel" ]
          , button
              Button.Primary
              (uncurry submit <$> filter (\_ -> canRestore) result)
              []
              [ HH.text "Restore Wallet" ]
          ]
      ]
