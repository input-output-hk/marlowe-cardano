module Main where

import qualified Data.ByteString.Short as SBS
import Language.Marlowe.Scripts.OpenRole (openRoleValidatorBytes, openRoleValidatorHash)

main :: IO ()
main = do
  putStrLn "Open Role Validator Hash:"
  print openRoleValidatorHash

  putStrLn "Open Role Validator size in bytes:"
  print $ SBS.length openRoleValidatorBytes
