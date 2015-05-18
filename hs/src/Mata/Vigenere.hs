module Mata.Vigenere where

import Prelude

import Data.List
import Data.List.Split

import Mata.XOR
import Mata.Hamming

keysizes = [2..40]

sortKeysizes :: String -> [(Int, Int)]
sortKeysizes ct = sort distances where
  rankSize s = do
               let [one, two] = take 2 (chunksOf s ct)
               ((hamming one two) `div` s, s)
  distances = Prelude.map rankSize keysizes
