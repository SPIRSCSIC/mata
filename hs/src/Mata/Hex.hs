{- Tools for dealing with data encoded as hexadecimal.
-}

module Mata.Hex where

import Data.HexString
import Data.ByteString
import Data.Text.Encoding (encodeUtf8)

{- Dump a hex string to a normal bytestring, or something. -}
dumpHex :: HexString -> ByteString
dumpHex = encodeUtf8 . toText

{- Convert a string of bytes to their hex form.
hexlify "\xfe\xed\xfa\xce" = "feedface"
-}
hexlify :: ByteString -> HexString
hexlify = fromBytes

{- Convert a hex-encoded string such as "feedface" to its raw bytes,
in this case "\xfe\xed\xfa\xce"
-}
unhexlify :: HexString -> ByteString
unhexlify = toBytes
