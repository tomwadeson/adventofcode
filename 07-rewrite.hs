module Day7 where

import Data.Word
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isLetter, isDigit)

data Node = Wire Input Label
          | Operator Gate Label
          deriving (Show)

data Gate = And Input Input
          | Or Input Input
          | Not Input
          | LShift Int Input
          | RShift Int Input
          deriving (Show)

data Input = Output Label
           | Signal Word16
           deriving (Show)

newtype Label = Label String deriving (Show)

parseNodes :: ReadP [Node]
parseNodes = sepBy parseNode $ satisfy (`elem` "\r\n")

parseNode :: ReadP Node
parseNode = parseWire <|> parseOperator

parseWire :: ReadP Node
parseWire = do
  input <- parseInput
  parseArrow
  label <- parseLabel
  return $ Wire input label

parseOperator :: ReadP Node
parseOperator = do
  gate  <- parseGate
  parseArrow
  label <- parseLabel
  return $ Operator gate label

parseGate :: ReadP Gate
parseGate =  parseAnd
         <|> parseOr
         <|> parseNot
         <|> parseLShift
         <|> parseRShift

parseAnd :: ReadP Gate
parseAnd = do
  i1 <- parseInput
  string " AND "
  i2 <- parseInput
  return $ And i1 i2

parseOr :: ReadP Gate
parseOr = do
  i1 <- parseInput
  string " OR "
  i2 <- parseInput
  return $ Or i1 i2

parseNot :: ReadP Gate
parseNot = do
  string "NOT "
  i1 <- parseInput
  return $ Not i1

parseLShift :: ReadP Gate
parseLShift = do
  i1 <- parseInput
  string " LSHIFT "
  shift <- parseInt
  return $ LShift shift i1

parseRShift :: ReadP Gate
parseRShift = do
  i1 <- parseInput
  string " RSHIFT "
  shift <- parseInt
  return $ RShift shift i1

parseInput :: ReadP Input
parseInput = parseOutput <|> parseSignal

parseSignal :: ReadP Input
parseSignal = do
  signal <- parseWord16
  return $ Signal signal

parseOutput :: ReadP Input
parseOutput = do
  label <- parseLabel
  return $ Output label

parseLabel :: ReadP Label
parseLabel = Label <$> munch1 (isLetter)

parseArrow :: ReadP String
parseArrow = string " -> "

parseWord16 :: ReadP Word16
parseWord16 = read <$> parseDigits

parseInt :: ReadP Int
parseInt = read <$> parseDigits

parseDigits :: ReadP String
parseDigits = many1 (satisfy isDigit)

main :: IO ()
main = do
  nodes <- fst . last . readP_to_S parseNodes <$> getContents
  mapM_ print nodes
