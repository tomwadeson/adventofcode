module Day6 where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Applicative

data Instruction = Toggle Range
                 | TurnOff Range
                 | TurnOn Range
                 deriving (Eq, Show)

data Range = Range Coordinate Coordinate
           deriving (Eq, Show)

type Coordinate = (Int, Int)

type Bulb = Bool

type Grid = [[Bulb]]

mkGrid :: Int -> Int -> Grid
mkGrid w h = replicate h . replicate w $ False

answer :: Grid -> [Instruction] -> Grid
answer = foldr (flip modifyLights)

modifyLights :: Grid -> Instruction -> Grid
modifyLights g i = map doRow $ zip g [0..]
  where
    doRow (row, x) = fst . foldr (\b (acc, y) ->
                             let b' = apply i (x, y) b
                                 y' = y+1
                             in  (b' : acc, y')) ([], 0) $ row

apply :: Instruction -> Coordinate -> Bulb -> Bulb
apply (Toggle r) c  = if inRange r c then not else id
apply (TurnOn r) c  = if inRange r c then const True else id
apply (TurnOff r) c = if inRange r c then const False else id

inRange :: Range -> Coordinate -> Bool
inRange (Range (x1, y1) (x2, y2)) (cx, cy) =
  cx >= x1 && cx <= x2 && cy >= y1 && cy <= y2

turnedOn :: Grid -> Int
turnedOn = sum . map (length . filter (== True))

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
  instructions <- (fst . last . readP_to_S parseInstructions) <$> getContents
  let grid = answer (mkGrid 1000 1000) instructions
  let numOfLightsOn = turnedOn grid
  putStrLn $ "Lights on: " ++ show numOfLightsOn

