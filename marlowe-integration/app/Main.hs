module Main
  where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Test.Integration.Marlowe (withLocalMarloweRuntime)
import Test.Integration.Marlowe.Script

data Escrow v = Escrow
  { buyer :: Party v
  , seller :: Party v
  , mediator :: Party v
  , price :: Integer
  }

main :: IO ()
main = withLocalMarloweRuntime $ runMarloweScript do
  alice <- allocateWallet "alice"
  bob <- allocateWallet "bob"
  william <- allocateWallet "william"

  (contractRef, Escrow{..}) <- submit alice $ create "close" $ buildV1Contract do
    buyer <- allocateParty alice $ ByRole "buyer"
    seller <- allocateParty bob $ ByRole "seller"
    mediator <- allocateParty william ByAddress
    let everythingIsAlright = mkChoiceId "Everything is alright" buyer
    let reportProblem = mkChoiceId "Report problem" buyer
    let confirmProblem = mkChoiceId "Confirm problem" seller
    let disputeProblem = mkChoiceId "Dispute problem" seller
    let dismissClaim = mkChoiceId "Dismiss claim" mediator
    let confirmClaim = mkChoiceId "Confirm claim" mediator
    let price = 10_000_000

    (,Escrow{..}) <$> when_
      [ ( adaIsDeposited seller buyer $ V1.Constant price
        , when_
            [ ( enumChoiceIsMade everythingIsAlright 0
              , close
              )
            , ( enumChoiceIsMade reportProblem 1
              , payAda seller (toAccount buyer) (V1.Constant price)
              $ when_
                  [ ( enumChoiceIsMade confirmProblem 0
                    , close
                    )
                  , ( enumChoiceIsMade disputeProblem 1
                    , when_
                        [ ( enumChoiceIsMade dismissClaim 0
                          , payAda buyer (toAccount seller) (V1.Constant price) close
                          )
                        , ( enumChoiceIsMade confirmClaim 1
                          , close
                          )
                        ]
                        240
                        close
                    )
                  ]
                  180
                  close
              )
            ]
            120
            close
        )
      ]
      60
      close

  liftIO $ putStrLn "1"

  submit alice $ applyInputs contractRef $ buildV1ApplyInputs buyer do
    depositAda seller price

  liftIO $ putStrLn "2"

  submit alice $ applyInputs contractRef $ buildV1ApplyInputs buyer do
    choose "Everything is alright" 0

  liftIO $ putStrLn "3"
