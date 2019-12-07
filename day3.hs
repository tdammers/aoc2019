module Main where

import AOCUtils
import Data.List
import Data.Maybe
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function

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

traceLeg :: Int -> (Int, Int) -> Leg -> (Map (Int, Int) Int, ((Int, Int), Int))
traceLeg d0 (x, y) (Leg dir d) =
  let (dx, dy) = case dir of
        L -> (-1, 0)
        R -> (1, 0)
        U -> (0, -1)
        D -> (0, 1)
      points = [ ((x + n * dx, y + n * dy), d0 + n)
               | n <- [1..d]
               ]
  in ( Map.fromList points
     , last points
     )

traceWire :: Int -> (Int, Int) -> Wire -> (Map (Int, Int) Int, ((Int, Int), Int))
traceWire d0 p0 [] = (Map.empty, (p0, d0))
traceWire d0 p0 (l:ls) =
  let (s1, (p1, d1)) = traceLeg d0 p0 l
      (s2, vn) = traceWire d1 p1 ls
  in (s1 <> s2, vn)

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

getWireCrossings :: [Wire] -> Map (Int, Int) Int
getWireCrossings wires =
  let traces = map (fst . traceWire 0 (0,0)) wires
  in case traces of
    x:xs -> foldl' (Map.intersectionWith (+)) x xs
    [] -> Map.empty

manhattan :: (Int, Int) -> Int
manhattan (x,y) = abs x + abs y

main = do
  wires <- loadWires "day3.input"
  let crossings = getWireCrossings wires
  -- part 1
  print . Map.findMin . Map.mapKeys manhattan $ crossings
  -- part 2
  print . minimumBy (compare `on` snd) . Map.toList $ crossings
