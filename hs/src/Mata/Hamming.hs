module Mata.Hamming where

import Data.Bits
import Data.Char

numberOfSetBits :: Int -> Int
numberOfSetBits x
    | x == 0    = 0
    | otherwise = 1 + (numberOfSetBits (x .&. (x - 1)))

hamming :: [Char] -> [Char] -> Int
hamming a b
  | (length a) == (length b) = sum (map (\ (x, y) -> numberOfSetBits (xor (ord x) (ord y))) (zip a b))
  | otherwise                = error "Lengths of both strings must be equal."
