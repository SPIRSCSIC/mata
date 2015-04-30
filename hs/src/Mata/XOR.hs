module Mata.XOR where

import Data.Bits
import Data.ByteString hiding (pack, unpack)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.HexString
import Data.Binary

import Mata.Hex

{- xor two bytestrings together -}
xor :: ByteString -> ByteString -> ByteString
xor x y = toStrict $ encode (Data.Bits.xor _x _y) where
  _x = (decode $ fromStrict x) :: Integer
  _y = (decode $ fromStrict y) :: Integer
