module Data.Address.Bech32.DataPart.CodePoint where

import Prologue

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum
  ( class BoundedEnum
  , class Enum
  , defaultPred
  , defaultSucc
  , fromEnum
  , toEnum
  )
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String.CodePoints (codePointFromChar)

data DataPartCodePoint
  = DP0
  -- no "1"
  | DP2
  | DP3
  | DP4
  | DP5
  | DP6
  | DP7
  | DP8
  | DP9
  | DPA
  -- no "b"
  | DPC
  | DPD
  | DPE
  | DPF
  | DPG
  | DPH
  -- no "i"
  | DPJ
  | DPK
  | DPL
  | DPM
  | DPN
  -- no "o"
  | DPP
  | DPQ
  | DPR
  | DPS
  | DPT
  | DPU
  | DPV
  | DPW
  | DPX
  | DPY
  | DPZ

derive instance Generic DataPartCodePoint _
derive instance Eq DataPartCodePoint
derive instance Ord DataPartCodePoint

instance Show DataPartCodePoint where
  show = genericShow

instance Bounded DataPartCodePoint where
  bottom = genericBottom
  top = genericTop

instance Enum DataPartCodePoint where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance BoundedEnum DataPartCodePoint where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

fromCodePoint :: CodePoint -> Maybe DataPartCodePoint
fromCodePoint cp = case fromEnum cp of
  0x30 -> Just DP0
  0x32 -> Just DP2
  0x33 -> Just DP3
  0x34 -> Just DP4
  0x35 -> Just DP5
  0x36 -> Just DP6
  0x37 -> Just DP7
  0x38 -> Just DP8
  0x39 -> Just DP9
  0x61 -> Just DPA
  0x63 -> Just DPC
  0x64 -> Just DPD
  0x65 -> Just DPE
  0x66 -> Just DPF
  0x67 -> Just DPG
  0x68 -> Just DPH
  0x6A -> Just DPJ
  0x6B -> Just DPK
  0x6C -> Just DPL
  0x6D -> Just DPM
  0x6E -> Just DPN
  0x70 -> Just DPP
  0x71 -> Just DPQ
  0x72 -> Just DPR
  0x73 -> Just DPS
  0x74 -> Just DPT
  0x75 -> Just DPU
  0x76 -> Just DPV
  0x77 -> Just DPW
  0x78 -> Just DPX
  0x79 -> Just DPY
  0x7A -> Just DPZ
  _ -> Nothing

toCodePoint :: DataPartCodePoint -> CodePoint
toCodePoint = codePointFromChar <<< case _ of
  DP0 -> '0'
  DP2 -> '2'
  DP3 -> '3'
  DP4 -> '4'
  DP5 -> '5'
  DP6 -> '6'
  DP7 -> '7'
  DP8 -> '8'
  DP9 -> '9'
  DPA -> 'a'
  DPC -> 'c'
  DPD -> 'd'
  DPE -> 'e'
  DPF -> 'f'
  DPG -> 'g'
  DPH -> 'h'
  DPJ -> 'j'
  DPK -> 'k'
  DPL -> 'l'
  DPM -> 'm'
  DPN -> 'n'
  DPP -> 'p'
  DPQ -> 'q'
  DPR -> 'r'
  DPS -> 's'
  DPT -> 't'
  DPU -> 'u'
  DPV -> 'v'
  DPW -> 'w'
  DPX -> 'x'
  DPY -> 'y'
  DPZ -> 'z'
