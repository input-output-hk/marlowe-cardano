module Page.Welcome.Forms.CreateWalletForm where

import Prologue

import AppM (passphrase) as AppM
import Capability.Marlowe (class ManageMarlowe, createWallet)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Set (Set)
import Data.Tuple.Nested ((/\))
import Data.Variant (match) as Variant
import Data.WalletNickname (WalletNickname)
import Effect.Aff.Class (class MonadAff)
import Forms as Forms
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.Component as FC
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import Page.Welcome.Forms.Render (render)
import Page.Welcome.Types (Action(..), Card(..), CreateWalletStep(..))
import Page.Welcome.Types as Welcome
import Type.Proxy (Proxy(..))

type Input = Set WalletNickname

type CreateWalletInput = String

type WalletOutput = Tuple WalletNickname MnemonicPhrase

type Component q m =
  H.Component q Input Welcome.Action m

initialInput :: CreateWalletInput
initialInput = ""

component
  :: forall q m
   . MonadAff m
  => MonadRec m
  => ManageMarlowe m
  => Component q m
component = Hooks.component \{ outputToken } used -> Hooks.do
  result /\ putResult <- usePutState Nothing
  creating /\ putCreating <- usePutState false
  serverError /\ putServerError <- usePutState ""
  form <- Hooks.captures { creating, serverError, used } Hooks.useMemo \_ ->
    FC.component
      { form: Forms.walletNickname used
      , formClasses: [ "relative", "space-y-4" ]
      }
  let
    submit walletNickname = do
      putServerError ""
      putCreating true
      response <- lift $ createWallet walletNickname AppM.passphrase
      putCreating false
      case response of
        Left err -> do
          let
            err' = err `flip Variant.match`
              { serverError: const
                  "We have encountered some serious problem. Please try again later."
              , clientServerError: const
                  "Unable to connect to server. Please check your internet connection."
              }
          putServerError err'

        Right newWalletDetails ->
          Hooks.raise outputToken $ OpenCard $ CreateWalletCard $
            CreateWalletPresentMnemonic newWalletDetails
  Hooks.pure $ render
    { body:
        [ HH.slot (Proxy :: _ "form") unit form initialInput case _ of
            FC.Updated res -> putResult res
            FC.Raised welcomeAction -> Hooks.raise outputToken welcomeAction
        , HH.p [ classNames Css.inputError ] [ HH.text serverError ]
        ]
    , inProgress: creating
    , onCancel:
        { action: Just $ Hooks.raise outputToken Welcome.CloseCard
        , label: "Cancel"
        }
    , onSkip: Nothing
    , onSubmit: { action: submit <$> result, label: "Create" }
    , title: "Create testnet wallet"
    }
