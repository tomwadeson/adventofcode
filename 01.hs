{-# LANGUAGE LambdaCase #-}

module Day1 where

import Prelude hiding (floor)
import Data.Maybe (fromMaybe)

data Bracket = Open
             | Close
             deriving (Show)

fromChar :: Char -> Maybe Bracket
fromChar '(' = Just Open
fromChar ')' = Just Close
fromChar _   = Nothing

fromString :: String -> Maybe [Bracket]
fromString = traverse fromChar

floor :: [Bracket] -> Int
floor = foldr toInstruction 0
  where
    toInstruction = \case { Open -> succ; Close -> pred; }

answer :: String -> String
answer str = let f = (show . floor) <$> fromString str
             in  fromMaybe "Bad input" f
