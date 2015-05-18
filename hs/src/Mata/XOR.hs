module Mata.XOR where

import Prelude

import Data.Bits
import Data.ByteString
import qualified Data.ByteString.Char8 as Char8
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
  _y = Data.ByteString.head $ Char8.pack [y]
  f = Data.Bits.xor _y

{- pad a string to a given length by repeating it -}
reppad :: String -> Int -> String
reppad x l = Prelude.take l (Prelude.concat $ Prelude.repeat x)

{- xor a string with an optionally shorter key by padding with repetition -}
repxor :: ByteString -> ByteString -> ByteString
repxor one k = Mata.XOR.xor one key where
  key = Char8.pack $ reppad (Char8.unpack k) (Char8.length one)
