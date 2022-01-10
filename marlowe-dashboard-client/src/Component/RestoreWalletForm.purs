module Componenet.RestoreWalletForm where

import Prologue

import Component.Button.Types as Button
import Component.Button.View (button)
import Component.Contacts.Types (WalletDetails)
import Data.MnemonicPhrase (class CheckMnemonic, MnemonicPhrase)
import Data.Set (Set)
import Data.Tuple (uncurry)
import Data.WalletNickname (WalletNickname)
import Debug (traceM)
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
  | Restored WalletDetails

type Component q m =
  H.Component q Input Msg m

component :: forall q m. MonadAff m => CheckMnemonic m => Component q m
component = Hooks.component \{ outputToken } input -> Hooks.do
  form <- Hooks.captures { input } Hooks.useMemo \_ ->
    Form.split (Forms.walletNickname input) Forms.mnemonicPhrase
  { result, html } <- useForm form $ Tuple "" $ AsyncInput "" NotAsked
  let cancel = Hooks.raise outputToken Closed
  let
    confirm nickname mnemonic = do
      traceM nickname
      traceM mnemonic
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
      , HH.div
          [ classNames [ "flex", "gap-4" ] ]
          [ button
              Button.Secondary
              (Just cancel)
              []
              [ HH.text "Cancel" ]
          , button
              Button.Primary
              (uncurry confirm <$> result)
              []
              [ HH.text "Restore Wallet" ]
          ]
      ]
