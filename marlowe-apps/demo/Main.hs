-- editorconfig-checker-disable-file

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Data.Aeson (encodeFile)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import Plutus.V2.Ledger.Api (POSIXTime(..))
import System.Environment (getArgs)

makeContract
  :: POSIXTime
  -> Party
  -> Contract
makeContract timeout party =
  When
    [
      Case (Choice (ChoiceId "Amount" party) [Bound 1 2000000])
        $ When
        [
           Case (Deposit party party (Token "" "") (ChoiceValue $ ChoiceId "Amount" party))
             $ makeChoices timeout "Folio"
             $ makeChoices timeout "Play"
             $ makeChoices timeout "Act"
             $ makeChoices timeout "Scene"
             $ makeChoices timeout "Line"
             $ makeChoices timeout "Word"
             $ makeChoices timeout "Letter"
             Close
        ]
        timeout
        Close
    ]
    timeout
    Close


makeChoices
  :: POSIXTime
  -> ChoiceName
  -> Contract
  -> Contract
makeChoices timeout choiceName continuation =
  When
    [
      Case (Notify TrueObs) continuation
    , Case (Choice (ChoiceId choiceName $ Role "c.marlowe"    ) [Bound 0 100]) continuation
    , Case (Choice (ChoiceId choiceName $ Role "e.cary"       ) [Bound 0 100]) continuation
    , Case (Choice (ChoiceId choiceName $ Role "f.beaumont"   ) [Bound 0 100]) continuation
    , Case (Choice (ChoiceId choiceName $ Role "j.lumley"     ) [Bound 0 100]) continuation
    , Case (Choice (ChoiceId choiceName $ Role "j.webster"    ) [Bound 0 100]) continuation
    , Case (Choice (ChoiceId choiceName $ Role "m.herbert"    ) [Bound 0 100]) continuation
    , Case (Choice (ChoiceId choiceName $ Role "w.shakespeare") [Bound 0 100]) continuation
    ]
    timeout
    Close


main :: IO ()
main =
  do
    [address, now] <- getArgs
    let
      readAddress = uncurry Address . fromJust . deserialiseAddressBech32 . pack
      deadline = POSIXTime $ read now + 2 * 3600 * 1000
      contract = makeContract deadline $ readAddress address
    encodeFile "contract.json" contract
