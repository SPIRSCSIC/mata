module Mata.Base64 where

import Data.ByteString hiding (pack)
import Data.ByteString.Base64 (encode, decode)
import Data.ByteString.Char8 (pack)
import Data.HexString

import Mata.Hex

{- Encodes whatever string you give it as Base64.
No care is taken for hex strings, for that case see b64HexEncode.
-}
b64Encode :: ByteString -> ByteString
b64Encode = encode

{- Encode a hex string into Base 64.
This first decodes the hexadecimal into raw bytes, then encodes with base 64.
For instance, both strings "aaaa" and "AAAA" will encode to "qqo="
-}
b64HexEncode :: HexString -> ByteString
b64HexEncode = encode . unhexlify

{- Decodes a Base64 string to a ByteString. That's it. -}
b64Decode :: ByteString -> ByteString
b64Decode x = case decode x of
  Left a -> pack a
  Right a -> a
