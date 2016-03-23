module Day6 where

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
turnedOn = countProp (== True)

turnedOff :: Grid -> Int
turnedOff = countProp (== False)

countProp :: (Bool -> Bool) -> Grid -> Int
countProp p g = sum . map (length . filter p) $ g
