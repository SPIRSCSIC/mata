module Mata.Vigenere where

import Prelude

import Data.ByteString.Char8 (pack, unpack)
import Data.List
import Data.List.Split

import Mata.XOR
import Mata.English
import Mata.Hamming

keybytes = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
keysizes = [2..40]

sortKeysizes :: String -> [(Int, Int)]
sortKeysizes ct = sort distances where
  rankSize s = do
               let [one, two] = take 2 (chunksOf s ct)
               ((hamming one two) `div` s, s)
  distances = Prelude.map rankSize keysizes

crackVigenere ct = do
  let keysize = head $ sortKeysizes ct
  let blocks = transpose $ chunksOf (snd keysize) ct
  let solved_blocks = Prelude.map (\x -> crackSingleCharKey x keybytes) blocks
  Prelude.map fst solved_blocks
