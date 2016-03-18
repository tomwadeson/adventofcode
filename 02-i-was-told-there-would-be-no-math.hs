module Day2 where

import Prelude hiding (length)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Box = Box { length :: Int, width :: Int, height :: Int }
           deriving (Show)

sides :: Box -> [Int]
sides (Box l w h) = [l*w, h*w, h*l]

surfaceArea :: Box -> Int
surfaceArea = sum . map (*2) . sides

smallestSide :: Box -> Int
smallestSide = minimum . sides

wrappingPaper :: Box -> Int
wrappingPaper b = surfaceArea b + smallestSide b

parseBoxes :: ReadP [Box]
parseBoxes = do sepBy parseBox $ satisfy (`elem` "\r\n\t ")

parseBox :: ReadP Box
parseBox = do l <- many1 digit
              char 'x'
              w <- many1 digit
              char 'x'
              h <- many1 digit
              return (Box (read l) (read w) (read h))

digit :: ReadP Char
digit = do satisfy isDigit

answer :: String -> Int
answer str = let boxes = fst . last . readP_to_S parseBoxes $ str
             in  sum . map wrappingPaper $ boxes

main :: IO ()
main = do
  str <- getContents
  putStrLn $ show (answer str)
