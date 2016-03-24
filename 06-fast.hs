module Day6 where

import qualified Data.Map.Strict as Map

data Instruction = Toggle { range :: Range }
                 | TurnOn { range :: Range }
                 | TurnOff { range :: Range }
                 deriving (Show)

data Range = Range { topLeft :: Coordinate, bottomRight :: Coordinate }
             deriving (Show)

type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate Bulb

type Bulb = Bool

expandCoords :: Range -> [Coordinate]
expandCoords = undefined

mkGrid :: Range -> Grid
mkGrid = (foldr (\x acc -> Map.insert x False acc) Map.empty) . expandCoords

turnedOn :: Grid -> Int
turnedOn = undefined

perform :: Grid -> Instruction -> Grid
perform g i = foldr changeBulb g (expandCoords . range $ i)
  where
    changeBulb = undefined

performAll :: Grid -> [Instruction] -> Grid
performAll = foldr (flip perform)
