module Day6 where

import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Foldable (foldl')

data Instruction = Toggle { range :: Range }
                 | TurnOn { range :: Range }
                 | TurnOff { range :: Range }
                 deriving (Show)

data Range = Range { topLeft :: Coordinate, bottomRight :: Coordinate }
             deriving (Show)

type Coordinate = (Int, Int)

type Grid = Set.Set Coordinate

type Bulb = Bool

expandCoords :: Range -> [Coordinate]
expandCoords (Range (x1, y1) (x2, y2)) = [ (x, y) | x <- [x1..x2], y <- [y1..y2] ]

mkGrid :: Grid
mkGrid = Set.empty

turnedOn :: Grid -> Int
turnedOn = Set.size

performAll :: Grid -> [Instruction] -> Grid
performAll = foldl' perform

perform :: Grid -> Instruction -> Grid
perform g i = foldl' (changeBulb i) g (expandCoords . range $ i)

changeBulb :: Instruction -> Grid -> Coordinate -> Grid
changeBulb (Toggle _) g c  = if Set.member c g then Set.delete c g else Set.insert c g
changeBulb (TurnOn _) g c  = Set.insert c g
changeBulb (TurnOff _) g c = Set.delete c g

parseInstructions :: ReadP [Instruction]
parseInstructions = 
  sepBy (parseToggle <|> parseTurnOn <|> parseTurnOff) $ satisfy (`elem` "\r\n\t ")

parseToggle :: ReadP Instruction
parseToggle = do
  string "toggle"
  range <- parseRange
  return (Toggle range)

parseTurnOn :: ReadP Instruction
parseTurnOn = do
  string "turn on"
  range <- parseRange
  return (TurnOn range)

parseTurnOff :: ReadP Instruction
parseTurnOff = do
  string "turn off"
  range <- parseRange
  return (TurnOff range)

parseRange :: ReadP Range
parseRange = do
  skipSpaces
  topLeft <- parseCoordinate
  string " through "
  bottomRight <- parseCoordinate
  return (Range topLeft bottomRight)

parseCoordinate :: ReadP Coordinate
parseCoordinate = do
  x <- parseInteger
  char ','
  y <- parseInteger
  return (x, y)

parseInteger :: ReadP Int
parseInteger = read <$> many1 (satisfy isDigit)

main :: IO ()
main = do
  let grid = mkGrid 
  instructions <- (fst . last . readP_to_S parseInstructions) <$> getContents
  let litBulbs = turnedOn (performAll grid instructions)
  putStrLn $ "Number of lit bulbs: " ++ show litBulbs
