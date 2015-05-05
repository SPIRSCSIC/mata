module Mata.English where

import Prelude
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.Map

import Mata.XOR

{- Frequency as percentage of each letter in the english alphabet.
The space character has a weight of 20% to give a bias to likely sentences.
-}
frequencies :: Map Char Float
frequencies = fromList [ ('E', 12.02), ('T', 9.10), ('A', 8.12),
                         ('O', 7.68), ('I', 7.31), ('N', 6.95),
                         ('S', 6.28), ('R', 6.02), ('H', 5.92),
                         ('D', 4.32), ('L', 3.98), ('U', 2.88),
                         ('C', 2.71), ('M', 2.61), ('F', 2.30),
                         ('Y', 2.11), ('W', 2.09), ('G', 2.03),
                         ('P', 1.82), ('B', 1.49), ('V', 1.11),
                         ('K', 0.62), ('X', 0.17), ('Q', 0.11),
                         ('J', 0.10), ('Z', 0.07), (' ', 20.0) ]

{- Attempt to rank a string's likelyhood of being English. -}
rank :: ByteString -> Float
rank x = (sum $ Prelude.map f (unpack x)) / len where
  f _x = case (Data.Map.lookup (toUpper _x) frequencies) of
             Just freq -> freq
             Nothing -> 0.0
  len = fromIntegral (Data.ByteString.length x)

{- Rank possible single byte repeated keys in a xor cipher
based on the likelyhood of the resulting plaintext being English.
-}
rankKeys :: ByteString -> [Char] -> Map Char Float
rankKeys x ys = map rank pts
  pts = map (xor1 x) ys
