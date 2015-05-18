module Set1 where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.ByteString.Char8 (pack)
import Data.HexString
import Data.List

import Mata.Base64
import Mata.Hex
import Mata.XOR
import Mata.English
import Mata.HTTP
import Mata.Hamming
import Mata.Vigenere

-- Challenge 1

testHexlify :: Assertion
testHexlify = assertEqual "hex encoding and decoding works"
              test (unhexlify $ hexlify test) where
                test = "\xfe\xed\xfa\xce"

testb64HexEncode :: Assertion
testb64HexEncode = assertEqual "base64 encoding hex strings works correctly."
                   (b64HexEncode test) correct where
                     test = hexString $ pack $ "aaaa"
                     correct = "qqo="

testb64Decode :: Assertion
testb64Decode = assertEqual "base64 decoding works correctly."
                (b64Decode test) _in where
                  test = "qqo="
                  _in = "\xaa\xaa"

testChallenge1 :: Assertion
testChallenge1 = assertEqual "I'm killing your brain like a poisonous mushroom"
                 (b64HexEncode _in) out where
                     _in = hexString $ pack $ "49276d206b696c6c696e6720796" ++
                           "f757220627261696e206c696b65206120706f69736f6e6" ++
                           "f7573206d757368726f6f6d"
                     out = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGE" ++
                           "gcG9pc29ub3VzIG11c2hyb29t"

-- Challenge 2

testXOR :: Assertion
testXOR = assertEqual "Basic XOR"
          (xor one one) three where
            one = "\xfe\xed\xfa\xce"
            three = "\x00\x00\x00\x00"

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
                      one = "aaaa"
                      three = "\x00\x00\x00\x00"

testRankEnglish :: Assertion
testRankEnglish = assertBool "English, yo"
                  ((rank one) >= 7.5 && (rank one) <= 7.7) where
                    one = "cooking mc's like a pound of bacon"

testChallenge3 = assertEqual "Challenge 3"
                 (crackSingleCharKey ct ks) cracked where
                   ks = ['a'..'z'] ++ ['A'..'Z']
                   ct = unhexlify $ hexString $ pack $ "1b37373331363f78151b" ++
                                                       "7f2b783431333d783978" ++
                                                       "28372d363c78373e783a" ++
                                                       "393b3736"
                   cracked = ('X', "Cooking MC's like a pound of bacon")

-- Challenge 4

testChallenge4 = do
  four <- getList "http://cryptopals.com/static/challenge-data/4.txt"
  let cts = Prelude.map (unhexlify . hexString . pack) four
  let keys = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  let pts = Prelude.map ((flip crackSingleCharKey) keys) cts
  let rankings = reverse $ sortOn fst $ Prelude.map (\x -> ((rank $ snd x), (snd x))) pts
  assertEqual "Challenge 4" (snd $ head $ rankings) "Now that the party is jumping\n"

-- Challenge 5

testChallenge5 = assertEqual "Challenge 5"
                 (hexlify $ repxor pt "ICE") ct where
                   pt = "Burning 'em, if you ain't quick and nimble\n" ++
                        "I go crazy when I hear a cymbal"
                   ct = hexString $ pack $ "0b3637272a2b2e63622c2e69692a2369" ++
                        "3a2a3c6324202d623d63343c2a26226324272765272a282b2f2" ++
                        "0430a652e2c652a3124333a653e2b2027630c692b2028316528" ++
                        "6326302e27282f"

-- Challenge 6

testHamming = assertEqual "test hamming distance function"
              (hamming "this is a test" "wokka wokka!!!") 37

testChallenge6 = do
  r <- get "http://cryptopals.com/static/challenge-data/6.txt"
  let ct = b64Decode r
  assertEqual "Challenge 6" (crackVigenere ct) "Terminator X: Bring the noise"

tests = [ testCase "Challenge 1" testChallenge1
        , testCase "hexlify" testHexlify
        , testCase "b64 hex encode" testb64HexEncode
        , testCase "b64 decode" testb64Decode
        , testCase "XOR" testXOR
        , testCase "Challenge 2" testChallenge2
        , testCase "XOR with 1 byte" testXORSingleByte
        , testCase "English ranking" testRankEnglish
        , testCase "Challenge 3" testChallenge3
        , testCase "Challenge 4" testChallenge4
        , testCase "Challenge 5" testChallenge5
        , testCase "Hamming distance" testHamming
        , testCase "Challenge 6" testChallenge6]
