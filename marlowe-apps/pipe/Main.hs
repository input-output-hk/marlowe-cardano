

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Control.Monad.Except (liftIO, runExceptT)
import Data.Aeson (eitherDecode, encode)
import Data.Text (Text)
import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Transact (handleWithEvents)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Explicit (addField, hoistEventBackend, idInjectSelector, subEventBackend, withEvent)
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import Observe.Event.Syntax ((≔))

import qualified Data.ByteString.Lazy.Char8 as LBS8 (getContents, lines, putStrLn, unpack)
import qualified Options.Applicative as O


main :: IO ()
main =
  do
    configParser <- getConfigParser
    config <-
      O.execParser
        $ O.info
          (O.helper {- <*> O.versionOption -} <*> configParser)
          (
            O.fullDesc
              <> O.progDesc "This command-line tool reads lines of JSON from standard input, interpets them as Marlowe App requests, executes them, and prints the response JSON on standard output."
              <> O.header "marlowe-pipe: run marlowe application requests"
          )
    eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
    requests <- LBS8.lines <$> LBS8.getContents
    sequence_
      [
        withEvent eventBackend (DynamicEventSelector "MarloweApp")
          $ \event ->
            do
              addField event $ ("line" :: Text) ≔ LBS8.unpack line
              let subBackend = subEventBackend idInjectSelector event eventBackend
              case eitherDecode line of
                Right request -> runExceptT (handleWithEvents (hoistEventBackend liftIO subBackend) "Handle" config request pure)
                                   >>= \case
                                    Right response -> LBS8.putStrLn $ encode response
                                    Left message -> LBS8.putStrLn $ encode message
                Left message -> LBS8.putStrLn $ encode message
      |
        line <- requests
      ]
