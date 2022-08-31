{-# LANGUAGE OverloadedStrings #-}
module Example where

import Language.Marlowe.Extended.V1

main :: IO ()
main = printJSON $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}

contract :: Contract
contract = Close
