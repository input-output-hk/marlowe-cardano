module Language.Marlowe.Object.BundlerSpec where

import Data.Functor (void)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Language.Marlowe.Object.Bundler (
  BundlerT,
  defineAction,
  defineContract,
  defineParty,
  defineToken,
  defineValue,
  runBundler_,
 )
import Language.Marlowe.Object.Gen ()
import Language.Marlowe.Object.Types
import Language.Marlowe.Object.TypesSpec (checkLaws)
import Test.Hspec
import Test.QuickCheck.Classes (applicativeLaws, eqLaws, functorLaws, monadLaws, monadZipLaws, showLaws)

spec :: Spec
spec = do
  describe "BundlerT" do
    checkLaws $ eqLaws $ Proxy @(BundlerT Maybe Word8)
    checkLaws $ showLaws $ Proxy @(BundlerT Maybe Word8)
    checkLaws $ functorLaws $ Proxy @(BundlerT Maybe)
    checkLaws $ applicativeLaws $ Proxy @(BundlerT Maybe)
    checkLaws $ monadLaws $ Proxy @(BundlerT Maybe)
    checkLaws $ monadZipLaws $ Proxy @(BundlerT Maybe)

    it "Can be used to define a simple contract" do
      let expected =
            ObjectBundle
              [ LabelledObject "ada" TokenType $ Token "" ""
              , LabelledObject "party1" PartyType $ Role "party1"
              , LabelledObject "party2" PartyType $ Role "party2"
              , LabelledObject "party1Amount" ValueType 20
              , LabelledObject "party2Amount" ValueType 10
              , LabelledObject "payout" ContractType $
                  Pay
                    (PartyRef "party1")
                    (Account (PartyRef "party2"))
                    (TokenRef "ada")
                    ( (AvailableMoney (PartyRef "party1") (TokenRef "ada") - AvailableMoney (PartyRef "party2") (TokenRef "ada")) `DivValue` 2
                    )
                    Close
              , LabelledObject "party1Deposit" ActionType $
                  Deposit (PartyRef "party1") (PartyRef "party1") (TokenRef "ada") (ValueRef "party1Amount")
              , LabelledObject "party2Deposit" ActionType $
                  Deposit (PartyRef "party2") (PartyRef "party2") (TokenRef "ada") (ValueRef "party2Amount")
              , LabelledObject "main" ContractType $
                  When
                    [ Case (ActionRef "party1Deposit") $ When [Case (ActionRef "party2Deposit") (ContractRef "payout")] 100 Close
                    , Case (ActionRef "party2Deposit") $ When [Case (ActionRef "party1Deposit") (ContractRef "payout")] 100 Close
                    ]
                    100
                    Close
              ]
      let actual = runBundler_ do
            ada <- defineToken "ada" $ Token "" ""
            party1 <- defineParty "party1" $ Role "party1"
            party2 <- defineParty "party2" $ Role "party2"
            party1Amount <- defineValue "party1Amount" 20
            party2Amount <- defineValue "party2Amount" 10
            let transferAmount = (AvailableMoney party1 ada - AvailableMoney party2 ada) `DivValue` 2
            payout <-
              defineContract "payout" $
                Pay party1 (Account party2) ada transferAmount Close
            party1Deposit <- defineAction "party1Deposit" $ Deposit party1 party1 ada party1Amount
            party2Deposit <- defineAction "party2Deposit" $ Deposit party2 party2 ada party2Amount
            void $
              defineContract "main" $
                When
                  [ Case party1Deposit $ When [Case party2Deposit payout] 100 Close
                  , Case party2Deposit $ When [Case party1Deposit payout] 100 Close
                  ]
                  100
                  Close
      actual `shouldBe` expected
