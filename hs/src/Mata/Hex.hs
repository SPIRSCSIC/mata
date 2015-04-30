{- Tools for dealing with data encoded as hexadecimal.
-}

module Mata.Hex where

import Data.HexString
import Data.ByteString
import Data.Text.Encoding (encodeUtf8)

{- Convert a string of bytes to their hex form.
hexlify "\xfe\xed\xfa\xce" = "feedface"
-}
hexlify :: ByteString -> ByteString
hexlify = encodeUtf8 . toText . fromBytes

{- Convert a hex-encoded string such as "feedface" to its raw bytes,
in this case "\xfe\xed\xfa\xce"
-}
unhexlify :: ByteString -> ByteString
unhexlify = toBytes . hexString
