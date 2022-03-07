module Test.Marlowe.Run.Simple where

import Prologue

import Data.HTTP.Method (Method(..))
import Data.String (Pattern(..), joinWith, split)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Test.Marlowe.Run (marloweRunTest)
import Test.Network.HTTP
  ( expectJsonContent
  , expectMethod
  , expectNextJsonRequest
  , expectUri
  )
import Test.Spec (Spec, describe)
import Test.Web.DOM.Assertions
  ( shouldBeDisabled
  , shouldCast
  , shouldHaveClass
  , shouldHaveText
  , shouldNotBeDisabled
  , shouldNotHaveClass
  )
import Test.Web.DOM.Query (findBy, getBy, name, role, text)
import Test.Web.Event.User (ShiftState(..), click, tab, type_)
import Test.Web.Monad (withContainer)
import Web.ARIA (ARIARole(..))

spec :: Spec Unit
spec = do
  describe "Marlowe Run" do
    marloweRunTest "starts without throwing an exception" do
      -- debugElement =<< getContainer
      -- logTestingPlaygroundURL
      pure unit

    marloweRunTest "allows you to generate a testnet wallet" do
      generateButton <- getBy role do
        name $ unsafeRegex "generate" ignoreCase
        pure Button
      click generateButton

      createDialog <- getBy role $ pure Dialog

      withContainer createDialog do
        nicknameField <- getBy role do
          name $ unsafeRegex "wallet nickname" ignoreCase
          pure Textbox
        createWalletButton <- shouldCast =<< getBy role do
          name $ unsafeRegex "create wallet" ignoreCase
          pure Button
        errorMessage <- getBy role $ pure Alert

        shouldBeDisabled createWalletButton
        errorMessage `shouldHaveClass` "invisible"

        tab ShiftNotPressed

        errorMessage `shouldNotHaveClass` "invisible"
        errorMessage `shouldHaveText` "Required."

        let nickname = "Nickname"

        click nicknameField
        type_ nicknameField nickname Nothing

        errorMessage `shouldHaveClass` "invisible"
        shouldNotBeDisabled createWalletButton

        let
          mnemonic = split (Pattern " ")
            "praise nut achieve misery coil shrimp post change view crumble taxi artwork hold list snap subject shiver actress video summer stone vicious trigger inmate"
          passphrase = "fixme-allow-pass-per-wallet"
          walletId = "b57c4784ed9c9d087053f6f04219aec82fca839a"
          pubKeyHash = "pk"
          address =
            "addr_test1qp8fta3c0g85dd9pu4fp4tsjq955w2zz76y2ywumsmky86nlxyxn4wsn2z3watr72naayj7kctvygade83cw98kd8gsqltffga"

        expectNextJsonRequest ado
          expectMethod POST
          expectUri "/api/wallet/v1/centralized-testnet/create"
          expectJsonContent
            { getCreatePassphrase: passphrase
            , getCreateWalletName: nickname
            }
          in
            { mnemonic
            , walletInfo:
                { walletId
                , pubKeyHash:
                    { unPaymentPubKeyHash: { getPubKeyHash: pubKeyHash } }
                , address
                }
            }

        click createWalletButton

        void $ findBy text $ pure $ joinWith " " mnemonic
