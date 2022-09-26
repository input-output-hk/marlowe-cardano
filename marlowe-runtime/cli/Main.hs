module Main
  where

import GHC.IO.Handle (hSetBuffering)
import Language.Marlowe.Runtime.CLI.Command
import System.IO (BufferMode(LineBuffering), stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options@Options{..} <- getOptions
  runCLIWithOptions options $ runCommand cmd
