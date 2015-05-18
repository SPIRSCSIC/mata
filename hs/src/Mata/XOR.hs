module Mata.XOR where

import Prelude

import Data.Char
import Data.Bits
import Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.HexString
import Data.Binary

import Mata.Hex

{- xor two strings together -}
xor :: String -> String -> String
xor x y = Prelude.map chr $ Prelude.zipWith Data.Bits.xor _x _y where
  _x = Prelude.map ord x
  _y = Prelude.map ord y

{- xor two known hexstrings together -}
hexxor :: HexString -> HexString -> HexString
hexxor x y = hexlify $ Mata.XOR.xor (unhexlify x) (unhexlify y)

{- xor all bytes in a string with one byte -}
xor1 :: String -> Char -> String
xor1 x y = Mata.XOR.xor x _y where
  _y = reppad [y] (Prelude.length x)

{- pad a string to a given length by repeating it -}
reppad :: String -> Int -> String
reppad x l = Prelude.take l (Prelude.concat $ Prelude.repeat x)

{- xor a string with an optionally shorter key by padding with repetition -}
repxor :: String -> String -> String
repxor one k = Mata.XOR.xor one key where
  key = reppad k (Prelude.length one)
