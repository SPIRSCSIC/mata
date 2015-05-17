module Set1 where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.ByteString.Char8 (pack)
import Data.HexString

import Mata.Base64
import Mata.Hex
import Mata.XOR
import Mata.English

-- Challenge 1

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

-- Challenge 2

testXOR :: Assertion
testXOR = assertEqual "Basic XOR"
          (xor one two) three where
            one = pack "\xfe\xed\xfa\xce"
            two = one
            three = pack "\x00\x00\x00\x00"

testChallenge2 :: Assertion
testChallenge2 = assertEqual "Fixed XOR"
                 (hexxor one two) three where
                   one = hexString $ pack "1c0111001f010100061a024b53535009181c"
                   two = hexString $ pack "686974207468652062756c6c277320657965"
                   three = hexString $ pack "746865206b696420646f6e277420706c6179"

-- Challenge 3

testXORSingleByte :: Assertion
testXORSingleByte = assertEqual "XOR with one byte"
                    (xor1 one 'a') three where
                      one = pack "aaaa"
                      three = pack "\x00\x00\x00\x00"

testRankEnglish :: Assertion
testRankEnglish = assertBool "English, yo"
                  ((rank one) >= 7.5 && (rank one) <= 7.7) where
                    one = pack "cooking mc's like a pound of bacon"

testChallenge3 = assertEqual "Challenge 3"
                 (crackSingleCharKey ct ks) cracked where
                   ks = ['a'..'z'] ++ ['A'..'Z']
                   ct = unhexlify $ hexString $ pack $ "1b37373331363f78151b" ++
                                                       "7f2b783431333d783978" ++
                                                       "28372d363c78373e783a" ++
                                                       "393b3736"
                   cracked = ('X', pack "Cooking MC's like a pound of bacon")


tests = [ testCase "Challenge 1" testChallenge1
        , testCase "hexlify" testHexlify
        , testCase "b64 hex encode" testb64HexEncode
        , testCase "b64 decode" testb64Decode

        , testCase "XOR" testXOR
        , testCase "Challenge 2" testChallenge2

        , testCase "XOR with 1 byte" testXORSingleByte
        , testCase "English ranking" testRankEnglish
        , testCase "Challenge 3" testChallenge3]
