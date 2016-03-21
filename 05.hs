module Day5 where

import Data.List (group, isInfixOf)

rule1 :: String -> Bool
rule1 = (>= 3) . length . filter (`elem` "aeiou")

rule2 :: String -> Bool
rule2 = (>= 1) . length . filter ((>= 2) . length) . group

rule3 :: String -> Bool
rule3 str = not $ "ab" `isInfixOf` str || "cd" `isInfixOf` str || "pq" `isInfixOf` str || "xy" `isInfixOf` str

isNice :: String -> Bool
isNice str = rule1 str && rule2 str && rule3 str

main :: IO ()
main = do
  content <- getContents
  let niceStrs = filter isNice . lines $ content
  putStrLn $ "Count of nice strings: " ++ (show . length $ niceStrs)
