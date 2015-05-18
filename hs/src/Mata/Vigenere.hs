module Mata.Vigenere where

import Prelude

import Data.List
import Data.List.Split
import Data.Char

import Mata.XOR
import Mata.English
import Mata.Hamming

keybytes = Prelude.map chr [0x00..0x7f]
keysizes = [3..40]

sortKeysizes :: String -> [(Float, Int)]
sortKeysizes ct = sort distances where
  ham x y = (fromIntegral (hamming x y)) :: Float
  rankSize s = do
               let [one, two, three, four] = take 4 (chunksOf s ct)
               let d1 = ham one two / fromIntegral s
               let d2 = ham three four / fromIntegral s
               let d3 = ham one three / fromIntegral s
               let d4 = ham two four / fromIntegral s
               (sum [d1,d2,d3,d4] / 4.0, s)
  distances = Prelude.map rankSize keysizes

crackVigenere :: String -> [Char]
crackVigenere ct = do
  let ks = snd $ head $ sortKeysizes ct
  let blocks = transpose $ chunksOf ks ct
  let solved_blocks = Prelude.map (\x -> crackSingleCharKey x keybytes) blocks
  Prelude.map fst solved_blocks
