module Main where

import AOCUtils
import Data.List
import Data.Maybe
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

data Direction
  = L
  | U
  | R
  | D
  deriving (Show, Read, Eq, Ord, Enum)

data Leg = Leg Direction Int
  deriving (Show)
      
type Wire = [Leg]

type Position = (Int, Int)

traceLeg :: (Int, Int) -> Leg -> (Set (Int, Int), (Int, Int))
traceLeg (x, y) (Leg L d) = (Set.fromList [ (x - n, y) | n <- [1..d] ], (x - d, y))
traceLeg (x, y) (Leg R d) = (Set.fromList [ (x + n, y) | n <- [1..d] ], (x + d, y))
traceLeg (x, y) (Leg U d) = (Set.fromList [ (x, y - n) | n <- [1..d] ], (x, y - d))
traceLeg (x, y) (Leg D d) = (Set.fromList [ (x, y + n) | n <- [1..d] ], (x, y + d))

traceWire :: (Int, Int) -> Wire -> (Set (Int, Int), (Int, Int))
traceWire p0 [] = (Set.empty, p0)
traceWire p0 (l:ls) =
  let (s1, p1) = traceLeg p0 l
      (s2, pn) = traceWire p1 ls
  in (s1 <> s2, pn)

loadWires :: FilePath -> IO [Wire]
loadWires fn = do
  lns <- lines <$> readFile fn
  forM lns $ \ln -> do
    case parseInput readLeg ln of
      (leg, "") -> return leg
      (_, str) -> error $ "Unexpected: " ++ show str
  where
    readLeg :: String -> Maybe (Leg, String)
    readLeg str = do
      (dir, _) <- listToMaybe $ reads (take 1 str)
      (dist, str'') <- listToMaybe $ reads (drop 1 str)
      let str''' = dropWhile (`notElem` "LURD") str''
      return (Leg dir dist, str''')

getWireCrossings :: [Wire] -> Set (Int, Int)
getWireCrossings wires =
  let traces = map (fst . traceWire (0,0)) wires
  in case traces of
    x:xs -> foldl' Set.intersection x xs
    [] -> Set.empty

manhattan :: (Int, Int) -> Int
manhattan (x,y) = abs x + abs y

main = do
  wires <- loadWires "day3.input"
  let crossings = getWireCrossings wires
  print . Set.findMin . Set.map manhattan $ crossings
