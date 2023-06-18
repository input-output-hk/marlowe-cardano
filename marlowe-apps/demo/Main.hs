-- editorconfig-checker-disable-file

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  , mainDemo
  , mainTidal
  ) where


import Control.Monad.Writer (Writer, runWriter)
import Data.Aeson (encodeFile)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Language.Marlowe.Core.V1.Merkle (Continuations, deepMerkleize)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import Plutus.V2.Ledger.Api (POSIXTime(..))
import System.Environment (getArgs)

import qualified Data.Map.Strict as M



main :: IO ()
main = mainTidal


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


mainDemo :: IO ()
mainDemo =
  do
    [address, now] <- getArgs
    let
      readAddress = uncurry Address . fromJust . deserialiseAddressBech32 . pack
      deadline = POSIXTime $ read now + 2 * 3600 * 1000
      contract = makeContract deadline $ readAddress address
    encodeFile "contract.json" contract


tidalContract
  :: POSIXTime
  -> Writer Continuations Contract
tidalContract timeout =
      tidalPatterns timeout
  =<< tidalSamples  timeout
  =<< tidalEffects  timeout
  =<< tidalEffects  timeout
  =<< tidalPatterns timeout
  =<< tidalSamples  timeout
  =<< tidalEffects  timeout
  =<< tidalEffects  timeout
      Close


tidalPatterns
  :: POSIXTime
  -> Contract
  -> Writer Continuations Contract
tidalPatterns timeout continuation =
  deepMerkleize
    $ When
    [
      Case (Notify TrueObs) continuation
    , Case (Choice (ChoiceId "bd hh sn hh"                       $ Role "sound") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "bd [bd cp] bd bd"                  $ Role "sound") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "<bd sn> <sd [cp cp]> <bd [cp cp]>" $ Role "sound") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "[kurt kurt] <jungbass industrial>" $ Role "sound") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "tabla*3 drum jvbass arpy"          $ Role "sound") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "supermandolin"                     $ Role "sound") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "superpiano"                        $ Role "sound") [Bound 1 4]) continuation
    ]
    timeout
    Close


tidalSamples
  :: POSIXTime
  -> Contract
  -> Writer Continuations Contract
tidalSamples timeout continuation =
  deepMerkleize
    $ When
    [
      Case (Notify TrueObs) continuation
    , Case (Choice (ChoiceId "c a f e"     $ Role "note") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "c4 a3 f6 e5" $ Role "note") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1"           $ Role "n"   ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1 2"         $ Role "n"   ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1 2 3 3"     $ Role "n"   ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1 2 3 4 "    $ Role "n"   ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "4 2 3 1"     $ Role "n"   ) [Bound 1 4]) continuation
    ]
    timeout
    Close


tidalEffects
  :: POSIXTime
  -> Contract
  -> Writer Continuations Contract
tidalEffects timeout continuation =
  deepMerkleize
    $ When
    [
      Case (Notify TrueObs) continuation
    , Case (Choice (ChoiceId "a e o"       $ Role "vowel"        ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1 0.7 0.5"   $ Role "gain"         ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1 1.5 2 0.5" $ Role "speed"        ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "<0.5 0.9>"   $ Role "delayfeedback") [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "0.6"         $ Role "room"         ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "1 2"         $ Role "legato"       ) [Bound 1 4]) continuation
    , Case (Choice (ChoiceId "0.2 0.5"     $ Role "resonance"    ) [Bound 1 4]) continuation
    ]
    timeout
    Close


mainTidal :: IO ()
mainTidal =
  do
    [now] <- getArgs
    let
      deadline = POSIXTime $ read now + 2 * 3600 * 1000
      (contract, continuations) = runWriter $ tidalContract deadline
    encodeFile "contract.json" contract
    encodeFile "continuations.json" $ M.mapKeys show continuations
