{- Tools for dealing with data encoded as hexadecimal.
-}

module Mata.Hex where

import Data.HexString
import Data.ByteString hiding (pack, unpack)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text.Encoding (encodeUtf8)

{- Dump a hex string to a normal string, or something. -}
dumpHex :: HexString -> String
dumpHex = unpack . encodeUtf8 . toText

{- Convert a string of bytes to their hex form.
hexlify "\xfe\xed\xfa\xce" = "feedface"
-}
hexlify :: String -> HexString
hexlify = fromBytes . pack

{- Convert a hex-encoded string such as "feedface" to its raw bytes,
in this case "\xfe\xed\xfa\xce"
-}
unhexlify :: HexString -> String
unhexlify = unpack . toBytes
