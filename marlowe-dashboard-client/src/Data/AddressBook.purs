module Data.AddressBook where

import Prologue

import Data.Address (Address)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Bimap (Bimap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.WalletNickname (WalletNickname)

newtype AddressBook = AddressBook (Bimap WalletNickname Address)

derive instance Generic AddressBook _
derive instance Newtype AddressBook _
derive instance Eq AddressBook
derive instance Ord AddressBook
derive newtype instance Show AddressBook
derive newtype instance DecodeJson AddressBook
derive newtype instance EncodeJson AddressBook
