module Componenet.RestoreWalletForm where

import Prologue

import API.Marlowe.Run.Wallet.CentralizedTestnet (RestoreError(..))
import Capability.Marlowe (class ManageMarlowe, restoreWallet)
import Component.Button.Types as Button
import Component.Button.View (button)
import Component.Progress.Circular as Progress
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.MnemonicPhrase (class CheckMnemonic, MnemonicPhrase)
import Data.Set (Set)
import Data.Tuple (uncurry)
import Data.WalletNickname (WalletNickname)
import Effect.Aff.Class (class MonadAff)
import Forms (MnemonicPhraseInput)
import Forms as Forms
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (AsyncInput(..))
import Halogen.Form as Form
import Halogen.Form.Component as FC
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))
import Page.Welcome.Types as Welcome
import Type.Proxy (Proxy(..))

type Input = Set WalletNickname

type RestoreWalletInput = Tuple String MnemonicPhraseInput

type RestoreWalletOutput = Tuple WalletNickname MnemonicPhrase

type Component q m =
  H.Component q Input Welcome.Action m

initialInput :: RestoreWalletInput
initialInput = Tuple "" $ AsyncInput "" NotAsked

component
  :: forall q m
   . MonadAff m
  => CheckMnemonic m
  => ManageMarlowe m
  => Component q m
component = Hooks.component \{ outputToken } used -> Hooks.do
  Tuple result resultId <- Hooks.useState Nothing
  Tuple restoring restoringId <- Hooks.useState false
  Tuple serverError serverErrorId <- Hooks.useState ""
  form <- Hooks.captures { restoring, serverError, used } Hooks.useMemo \_ ->
    FC.component
      { form: Form.split (Forms.walletNickname used) Forms.mnemonicPhrase
      , formClasses: [ "relative", "space-y-4" ]
      }
  let
    submit nickname mnemonic = do
      Hooks.put serverErrorId ""
      Hooks.put restoringId true
      response <- lift $ restoreWallet nickname mnemonic ""
      Hooks.put restoringId false
      case response of
        Left InvalidMnemonic ->
          Hooks.put serverErrorId "Invalid mnemonic phrase."
        Left _ ->
          Hooks.put serverErrorId "Error from server."
        Right walletDetails ->
          Hooks.raise outputToken $ Welcome.ConnectWallet nickname walletDetails
  Hooks.pure do
    HH.div [ classNames [ "p-5", "lg:p-6", "space-y-2" ] ]
      [ HH.h2
          [ classNames [ "font-bold" ] ]
          [ HH.text "Restore testnet wallet" ]
      , HH.slot (Proxy :: _ "form") unit form initialInput case _ of
          FC.Updated res -> Hooks.put resultId res
          FC.Raised welcomeAction -> Hooks.raise outputToken welcomeAction
      , HH.p_
          [ HH.b_ [ HH.text "IMPORTANT:" ]
          -- FIXME: as part of SCP-3173, Write a section in the Marlowe Run documentation and add a link to it
          , HH.text "Do not use a real wallet phrase <read more>"
          ]
      -- TODO replace with progress buttons when refactored.
      , HH.p [ classNames Css.inputError ] [ HH.text serverError ]
      , HH.div
          [ classNames [ "flex", "justify-center", "gap-4" ] ]
          if restoring then
            [ Progress.view Progress.defaultSpec
                { color = "text-purple"
                , width = "w-14"
                , height = "h-14"
                }
            ]
          else
            [ button
                Button.Secondary
                (Just $ Hooks.raise outputToken Welcome.CloseCard)
                [ "flex-1" ]
                [ HH.text "Cancel" ]
            , button
                Button.Primary
                (uncurry submit <$> result)
                [ "flex-1" ]
                [ HH.text "Restore Wallet" ]
            ]
      ]
