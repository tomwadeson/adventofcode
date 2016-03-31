module Day7 where

import Data.Word
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isLetter, isDigit)
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe, maybeToList, fromJust)
import Data.List (delete)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Node = Wire { input :: Input, label :: Label }
          | Operator { gate :: Gate, label ::  Label }
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

newtype Label = Label String deriving (Eq, Ord, Show)

type Graph = Map.Map Label [Label]

type NodeTable = Map.Map Label Node

toGraph :: [Node] -> Graph
toGraph = foldl' (\acc x -> Map.insertWith (++) (label x) (inputs x) acc) Map.empty

toNodeTable :: [Node] -> NodeTable
toNodeTable = foldl' (\acc x -> Map.insert (label x) x acc) Map.empty

inputs :: Node -> [Label]
inputs (Wire i l)                = maybeToList $ output i
inputs (Operator (And i1 i2) _)  = mapMaybe output [i1, i2]
inputs (Operator (Or i1 i2) _)   = mapMaybe output [i1, i2]
inputs (Operator (Not i) _)      = maybeToList $ output i
inputs (Operator (LShift _ i) _) = maybeToList $ output i
inputs (Operator (RShift _ i) _) = maybeToList $ output i

-- TODO: I don't like this name
output :: Input -> Maybe Label
output (Output l) = Just l
output _          = Nothing

-- Topological sort, using Kahn's algorithm.  Messy, unsafe, and inefficient implementation!
-- See: https://en.wikipedia.org/wiki/Topological_sorting#Kahn.27s_algorithm
topsort :: Graph -> [Label]
topsort g = topsort' g (roots g) []
  where
    topsort' g s l
      | null s    = l
      | otherwise = let n        = Set.elemAt 0 s
                        l'       = l ++ [n]
                        newRoots = Map.keysSet $ Map.filter (== [n]) g
                        s'       = Set.delete n $ Set.union s newRoots
                        g'       = Map.map (delete n) g
                    in  topsort' g' s' l'
    
    roots = Map.keysSet . Map.filter null

runCircuit :: [Node] -> Map.Map Label Word16
runCircuit nodes = let table              = toNodeTable nodes
                       dependencyOrdering = map (fromJust . (`Map.lookup` table)) . topsort . toGraph $ nodes
                   in  foldl' (flip execute) Map.empty dependencyOrdering

execute :: Node -> Map.Map Label Word16 -> Map.Map Label Word16
execute (Wire i l) m                 = Map.insert l (value i m) m
execute (Operator (And i1 i2) l) m   = Map.insert l (value i1 m .&. value i2 m) m
execute (Operator (Or i1 i2) l) m    = Map.insert l (value i1 m .|. value i2 m) m
execute (Operator (Not i1) l) m      = Map.insert l (complement (value i1 m)) m
execute (Operator (LShift s i1) l) m = Map.insert l (shiftL (value i1 m) s) m
execute (Operator (RShift s i1) l) m = Map.insert l (shiftR (value i1 m) s) m

value :: Input -> Map.Map Label Word16 -> Word16
value (Output l) m = fromJust $ Map.lookup l m
value (Signal s) _ = s

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
parseLabel = Label <$> munch1 isLetter

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
  let result = Map.toList $ runCircuit nodes
  mapM_ print result
