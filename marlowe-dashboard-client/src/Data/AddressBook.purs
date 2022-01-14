module Data.AddressBook where

import Prologue

import Data.Address (Address)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Bimap (Bimap)
import Data.Bimap as BM
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Unfoldable (class Unfoldable)
import Data.WalletNickname (WalletNickname)

newtype AddressBook = AddressBook (Bimap WalletNickname Address)

derive instance Generic AddressBook _
derive instance Newtype AddressBook _
derive instance Eq AddressBook
derive instance Ord AddressBook
derive newtype instance Show AddressBook
derive newtype instance DecodeJson AddressBook
derive newtype instance EncodeJson AddressBook

isEmpty :: AddressBook -> Boolean
isEmpty = BM.null <<< unwrap

empty :: AddressBook
empty = AddressBook BM.empty

insert :: WalletNickname -> Address -> AddressBook -> AddressBook
insert nickname = over AddressBook <<< BM.insert nickname

containsNickname :: WalletNickname -> AddressBook -> Boolean
containsNickname nickname = BM.memberL nickname <<< unwrap

containsAddress :: Address -> AddressBook -> Boolean
containsAddress address = BM.memberR address <<< unwrap

nicknames :: AddressBook -> Set WalletNickname
nicknames = BM.keysL <<< unwrap

addresses :: AddressBook -> Set Address
addresses = BM.keysR <<< unwrap

lookupNickname :: Address -> AddressBook -> Maybe WalletNickname
lookupNickname address = BM.lookupR address <<< unwrap

lookupAddress :: WalletNickname -> AddressBook -> Maybe Address
lookupAddress nickname = BM.lookupL nickname <<< unwrap

toUnfoldable
  :: forall t. Unfoldable t => AddressBook -> t (Tuple WalletNickname Address)
toUnfoldable = BM.toUnfoldable <<< unwrap
