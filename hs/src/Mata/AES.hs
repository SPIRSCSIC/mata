module Mata.AES where

import Crypto.Cipher.AES

import Data.ByteString hiding (pack, unpack, length)
import Data.ByteString.Char8 (pack, unpack)
import Data.List.Split
import Data.Set

{- Encrypt with AES in ECB mode -}
ecbEncrypt :: String -> String -> String
ecbEncrypt pt k = unpack $ encryptECB aes (pack pt) where
  aes = initAES (pack k)

{- Decrypt with AES in ECB mode -}
ecbDecrypt :: String -> String -> String
ecbDecrypt ct k = unpack $ decryptECB aes (pack ct) where
  aes = initAES (pack k)

{- Try to figure out if a ciphertext was encrypted with ECB mode -}
detectECB :: String -> Bool
detectECB ct = length unique < length chunks where
               chunks = chunksOf 16 ct
               unique = fromList chunks
