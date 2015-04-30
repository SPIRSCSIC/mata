module Set1 where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.ByteString.Char8 (pack)

import Mata.Base64
import Mata.Hex

testHexlify :: Assertion
testHexlify = assertEqual "wanker" test (unhexlify $ hexlify test) where
  test = pack "wanker"

testChallenge1 :: Assertion
testChallenge1 = assertEqual "I'm killing your brain like a poisonous mushroom"
                 (b64HexEncode _in) out where
                     _in = pack $ "49276d206b696c6c696e6720796f75722062726" ++
                           "1696e206c696b65206120706f69736f6e6f7573206d757" ++
                           "368726f6f6d"
                     out = pack $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGE" ++
                           "gcG9pc29ub3VzIG11c2hyb29t"

tests = [ testCase "Challenge 1" testChallenge1
        , testCase "hexlify" testHexlify]
