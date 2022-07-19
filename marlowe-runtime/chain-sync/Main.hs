module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Marlowe Chain Sync!"
  MyLib.someFunc
