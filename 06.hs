module Day6 where

data Instruction = Toggle Range
                 | TurnOff Range
                 | TurnOn Range
                 deriving (Eq, Show)

data Range = Range Coordinate Coordinate
           deriving (Eq, Show)

type Coordinate = (Int, Int)

type Grid = [[Bool]]

modifyLights :: Grid -> Instruction -> Grid
modifyLights g i = map snd . map doRow $ zip g [0..]
  where
    doRow (row, x) = undefined

inRange :: Range -> Coordinate -> Bool
inRange (Range x y) c = c >= x && x <= y  

turnedOn :: Grid -> Int
turnedOn = countProp (== True)

turnedOff :: Grid -> Int
turnedOff = countProp (== False)

countProp :: (Bool -> Bool) -> Grid -> Int
countProp p g = sum . map (length . filter p) $ g
