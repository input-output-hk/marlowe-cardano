module Main
  where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Test.Integration.Marlowe (withLocalMarloweRuntime)
import Test.Integration.Marlowe.Script

data Escrow v = Escrow
  { payer :: Party v
  , payee :: Party v
  , price :: Integer
  }

main :: IO ()
main = withLocalMarloweRuntime $ runMarloweScript do
    alice <- allocateWallet "alice"
    bob <- allocateWallet "bob"

    (contractRef, Escrow{..}) <- submit alice $ create "Test contract" $ buildV1Contract do
      payer <- allocateParty alice $ ByRole "payer"
      payee <- allocateParty bob $ ByRole "payee"
      let everythingIsAlright = mkChoiceId "Everything is alright" payer
      let reportProblem = mkChoiceId "Report problem" payer
      let price = 10_000_000

      (,Escrow{..}) <$> when_
        [ ( adaIsDeposited payee payer $ V1.Constant price
          , when_
              [ ( enumChoiceIsMade everythingIsAlright 0
                , close
                )
              , ( enumChoiceIsMade reportProblem 1
                , payAda payee (toAccount payer) (V1.Constant price) close
                )
              ]
              1200
              close
          )
        ]
        600
        close

    liftIO $ putStrLn "1"

    submit alice $ applyInputs contractRef $ buildV1ApplyInputs payer do
      depositAda payee price

    liftIO $ putStrLn "2"

    submit alice $ applyInputs contractRef $ buildV1ApplyInputs payer do
      choose "Everything is alright" 0

    liftIO $ putStrLn "3"
