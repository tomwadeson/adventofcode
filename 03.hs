module Day3 where

import Prelude hiding (Either(..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Direction = Up | Down | Left | Right
               deriving (Show)

type Location = (Int, Int)

fromChar :: Char -> Maybe Direction
fromChar '^' = Just Up
fromChar 'v' = Just Down
fromChar '<' = Just Left
fromChar '>' = Just Right
fromChar _   = Nothing

fromString :: String -> Maybe [Direction]
fromString = traverse fromChar

walk :: Location -> Direction -> Location
walk (x, y) Up    = (x, y+1)
walk (x, y) Down  = (x, y-1)
walk (x, y) Left  = (x-1, y)
walk (x, y) Right = (x+1, y)

follow :: Location -> [Direction] -> (Location, S.Set Location)
follow loc = foldr (\x (prev, acc) ->
  let next = walk prev x
  in  (next, S.insert next acc)) (loc, S.singleton loc)

answer :: String -> String
answer str = 
  let directions   = fromString str
      uniqueHouses = show . S.size . snd . follow' (0,0) <$> directions
  in  fromMaybe "Bad input" uniqueHouses
