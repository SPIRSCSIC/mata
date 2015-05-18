{-# LANGUAGE OverloadedStrings #-}

module Mata.HTTP where

import Control.Lens
import Control.Monad
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.List.Split

import Network.Wreq
import qualified Network.Wreq.Session as S


{- Send a simple HTTP GET to the given URL and return the response body. -}
get :: String -> IO String
get x = S.withSession $ \sess -> do
  r <- S.get sess x
  return $ (unpack .toStrict) $ r ^. responseBody

{- -}
getList :: String -> IO [String]
getList x = do
  r <- Mata.HTTP.get x
  return $ splitOn "\n" r
