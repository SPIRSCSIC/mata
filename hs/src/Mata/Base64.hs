module Mata.Base64 where

import Data.ByteString hiding (pack, unpack)
import Data.ByteString.Base64 (encode, decode)
import Data.ByteString.Char8 (pack, unpack)
import Data.HexString

import Mata.Hex

{- Encodes whatever string you give it as Base64.
No care is taken for hex strings, for that case see b64HexEncode.
-}
b64Encode :: String -> String
b64Encode = unpack . encode . pack

{- Encode a hex string into Base 64.
This first decodes the hexadecimal into raw bytes, then encodes with base 64.
For instance, both strings "aaaa" and "AAAA" will encode to "qqo="
-}
b64HexEncode :: HexString -> String
b64HexEncode = unpack . encode . pack . unhexlify

{- Decodes a Base64 string. That's it. -}
b64Decode :: String -> String
b64Decode x = case decode (pack x) of
  Left a -> a
  Right a -> unpack a
