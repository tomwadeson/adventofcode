module Day7 where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Data.List (delete)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Maybe (fromJust)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isLetter)
import Data.Word

data Node = Signal { label :: Label, value :: Word16 }
          | Operator { label :: Label, gate :: Gate }
          deriving (Show)

data Gate = And Label Label
          | Or Label Label
          | Not Label
          | LShift Label Int
          | RShift Label Int
          deriving (Show)

newtype Label = Label String deriving (Eq, Ord, Show)

type NodeTable = Map.Map Label Node

type Graph = Map.Map Label [Label]

--nodes :: [Node]
--nodes = [Signal (Label "x") 1, Signal (Label "y") 2, Operator (Label "z") (And (Label "x") (Label "y"))]

toNodeTable :: [Node] -> NodeTable
toNodeTable = foldl' (\acc x -> Map.insert (label x) x acc) Map.empty

toGraph :: [Node] -> Graph
toGraph nodes = toGraph' nodes Map.empty
  where
    toGraph' [] c     = c
    toGraph' (n:ns) c = case n of
      (Signal l _)   -> toGraph' ns (Map.insert l [] c)
      (Operator l g) -> toGraph' ns (Map.insertWith (++) l (inputs g) c)

inputs :: Gate -> [Label]
inputs (And x y)    = [x, y]
inputs (Or x y)     = [x, y]
inputs (Not x)      = [x]
inputs (LShift x _) = [x]
inputs (RShift x _) = [x]

-- Topological sort, using Kahn's algorithm.  Messy, unsafe, and inefficient implementation!
-- See: https://en.wikipedia.org/wiki/Topological_sorting#Kahn.27s_algorithm
topsort :: Graph -> [Label]
topsort g = topsort' g (roots g) []
  where
    topsort' g s l
      | null s    = l
      | otherwise = 
          let n              = Set.elemAt 0 s
              l'             = l ++ [n]
              (g', newRoots) = removeEdgesFrom n g
              s'             = Set.delete n $ Set.union s newRoots
          in  topsort' g' s' l'
    
    roots = Map.keysSet . Map.filter null

    removeEdgesFrom l g =
      let newRoots = Map.keysSet $ Map.filter (== [l]) g
          g'       = Map.map (delete l) g
      in  (g', newRoots)

runCircuit :: Graph -> NodeTable -> Map.Map Label Word16
runCircuit g nt = let nodes = map (\x -> fromJust (Map.lookup x nt)) . topsort $  g
                  in  foldl' (\acc x -> execute x acc) Map.empty nodes

-- TODO: refactor the duplication; this doesn't scale well
execute :: Node -> Map.Map Label Word16 -> Map.Map Label Word16
execute (Signal l v) r           = Map.insert l v r
execute (Operator l (And x y)) r = 
  let xVal = Map.lookup x r
      yVal = Map.lookup y r
      z    = (.&.) <$> xVal <*> yVal
  in  Map.insert l (fromJust z) r
execute (Operator l (Or x y))  r = 
  let xVal = Map.lookup x r
      yVal = Map.lookup y r
      z    = (.|.) <$> xVal <*> yVal
  in  Map.insert l (fromJust z) r
execute (Operator l (Not x ))  r = 
  let xVal = Map.lookup x r
      z    = (complement) <$> xVal
  in  Map.insert l (fromJust z) r
execute (Operator l (LShift x y))  r = 
  let xVal = Map.lookup x r
      z    = (`shiftL` y) <$> xVal
  in  Map.insert l (fromJust z) r
execute (Operator l (RShift x y))  r = 
  let xVal = Map.lookup x r
      z    = (`shiftR` y) <$> xVal
  in  Map.insert l (fromJust z) r

parseNodes :: ReadP [Node]
parseNodes = sepBy (parseSignal <|> parseAnd <|> parseOr <|> parseNot <|> parseLShift <|> parseRShift) $ satisfy (`elem` "\r\n")

parseSignal :: ReadP Node
parseSignal = do
  v <- parseWord16
  parseArrow
  l <- parseLabel
  return (Signal l v)

parseAnd :: ReadP Node
parseAnd = do
  x <- parseLabel
  string " AND "
  y <- parseLabel
  parseArrow
  z <- parseLabel
  return (Operator z (And x y))

parseOr :: ReadP Node
parseOr = do
  x <- parseLabel
  string " OR "
  y <- parseLabel
  parseArrow
  z <- parseLabel
  return (Operator z (Or x y))

parseNot :: ReadP Node
parseNot = do
  string "NOT "
  x <- parseLabel
  parseArrow
  z <- parseLabel
  return (Operator z (Not x))

parseLShift :: ReadP Node
parseLShift = do
  x <- parseLabel
  string " LSHIFT "
  s <- parseNumber
  parseArrow
  z <- parseLabel
  return (Operator z (LShift x s))

parseRShift :: ReadP Node
parseRShift = do
  x <- parseLabel
  string " RSHIFT "
  s <- parseNumber
  parseArrow
  z <- parseLabel
  return (Operator z (RShift x s))

parseWord16 :: ReadP Word16
parseWord16 = read <$> many1 (satisfy isDigit)

parseNumber :: ReadP Int
parseNumber = read <$> many1 (satisfy isDigit)

parseArrow :: ReadP String
parseArrow = string " -> "

parseLabel :: ReadP Label
parseLabel = Label <$> munch (isLetter)

main :: IO ()
main = do
  nodes <- (fst . last . readP_to_S parseNodes) <$> getContents
  print nodes
  let nodeTable = toNodeTable nodes
  let graph     = toGraph nodes
  let result    = runCircuit graph nodeTable
  print result
  putStrLn "If you're looking for 'a', it's:"
  print $ Map.lookup (Label "a") result

