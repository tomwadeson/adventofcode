{-# LANGUAGE OverloadedStrings #-}

-- Solution for day 4 of http://adventofcode.com/

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.MD5 (md5)

newtype Secret = Secret { getByteString :: LB.ByteString }

mkHash :: Secret -> Int -> LB.ByteString
mkHash s i = let counter = C.pack . show $ i
             in  LB.append (getByteString s) counter

mine :: Secret -> [(Int, String)]
mine s = map (\x -> (x, show . md5 $ mkHash s x)) [1..]

hit :: String -> Bool
hit = all (== '0') . take 5

answer :: [(Int, String)] -> (Int, String)
answer = head . filter (\(i, bs) -> hit bs)

main :: IO ()
main = do putStrLn "Secret key?"
          key <- (Secret . LB.fromStrict) <$> B.getLine
          print $ answer . mine $ key
          
