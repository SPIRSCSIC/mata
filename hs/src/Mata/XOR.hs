module Mata.XOR where

import Prelude
import Data.Bits
import Data.ByteString
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.HexString
import Data.Binary

import Mata.Hex

_xor :: [Word8] -> [Word8] -> [Word8]
_xor = Prelude.zipWith Data.Bits.xor

{- xor two bytestrings together -}
xor :: ByteString -> ByteString -> ByteString
xor x y = Data.ByteString.pack $ _xor _x _y where
  _x = (Data.ByteString.unpack x)
  _y = (Data.ByteString.unpack y)

{- xor two known hexstrings together -}
hexxor :: HexString -> HexString -> HexString
hexxor x y = hexlify $ Mata.XOR.xor (unhexlify x) (unhexlify y)

{- xor all bytes in a string with one byte -}
xor1 :: ByteString -> Char -> ByteString
xor1 x y = Data.ByteString.pack $ Prelude.map f _x where
  _x = (Data.ByteString.unpack x)
  _y = Data.ByteString.head $ Data.ByteString.Char8.pack [y]
  f = Data.Bits.xor _y
