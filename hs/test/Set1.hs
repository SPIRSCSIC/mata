module Set1 where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.ByteString.Char8 (pack)
import Data.HexString

import Mata.Base64
import Mata.Hex

testHexlify :: Assertion
testHexlify = assertEqual "hex encoding and decoding works"
              test (unhexlify $ hexlify test) where
                test = pack "\xfe\xed\xfa\xce"

testb64HexEncode :: Assertion
testb64HexEncode = assertEqual "base64 encoding hex strings works correctly."
                   (b64HexEncode test) correct where
                     test = hexString $ pack $ "aaaa"
                     correct = pack $ "qqo="

testb64Decode :: Assertion
testb64Decode = assertEqual "base64 decoding works correctly."
                (b64Decode test) _in where
                  test = pack "qqo="
                  _in = pack "\xaa\xaa"

testChallenge1 :: Assertion
testChallenge1 = assertEqual "I'm killing your brain like a poisonous mushroom"
                 (b64HexEncode _in) out where
                     _in = hexString $ pack $ "49276d206b696c6c696e6720796" ++
                           "f757220627261696e206c696b65206120706f69736f6e6" ++
                           "f7573206d757368726f6f6d"
                     out = pack $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGE" ++
                           "gcG9pc29ub3VzIG11c2hyb29t"

tests = [ testCase "Challenge 1" testChallenge1
        , testCase "hexlify" testHexlify
        , testCase "b64 hex encode" testb64HexEncode
        , testCase "b64 decode" testb64Decode
        ]
