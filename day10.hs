module Main
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
import Control.Monad
import Data.Function

inputToSet :: [[Char]] -> Set (Int, Int)
inputToSet rows =
  mconcat $ zipWith rowToSet [0..] rows

rowToSet :: Int -> [Char] -> Set (Int, Int)
rowToSet y row =
  mconcat $ zipWith (flip cellToSet y) [0..] row

cellToSet :: Int -> Int -> Char -> Set (Int, Int)
cellToSet x y '#' = Set.singleton (x, y)
cellToSet _ _ _ = Set.empty

(x, y) ^+^ (dx, dy) = (x + dx, y + dy)

(x, y) ^-^ (dx, dy) = (x - dx, y - dy)

(x, y) ^* s = (x * s, y * s)

(*^) = flip (^*)

-- linearly correlated:
-- x1/y1 == x2/y2
-- x1 * y2 == x2 * y1

linCorr :: (Int, Int) -> (Int, Int) -> Bool
linCorr (x1, y1) (x2, y2) =
  x1 * y2 == x2 * y1

sameOrientation :: (Int, Int) -> (Int, Int) -> Bool
sameOrientation (x1, y1) (x2, y2) =
  signum x1 == signum x2 && signum y1 == signum y2

countVisibleFrom :: Set (Int, Int) -> (Int, Int) -> Int
countVisibleFrom set pos =
  go set (Set.toList set)
  where
    go _ [] = 0
    go set (c:cs) =
      let d = c ^-^ pos
      in if (d /= (0,0)) && (c `Set.member` set) then
            let pred x = not (linCorr (x ^-^ pos) d) || not (sameOrientation (x ^-^ pos) d)
                set' = Set.filter pred set
                cs' = filter pred cs
            in succ $ go set' cs'
         else
            go set cs

printVisibleSet :: Int -> Set (Int, Int) -> Set (Int, Int) -> IO ()
printVisibleSet size set vis = do
  forM_ [0..size-1] $ \y -> do
    forM_ [0..size-1] $ \x -> do
      if (x, y) `Set.member` vis then
        putStr "#"
      else if (x, y) `Set.member` set then
        putStr "."
      else
        putStr " "
    putStrLn ""
  putStrLn ""


part1 :: Int -> Set (Int, Int) -> IO (Int, Int)
part1 size set = do
  let candidates = Set.toList set
  let mapped = zip candidates $ map (countVisibleFrom set) candidates
  -- mapM_ (\(coords, vis) -> printf "%s -> %d\n" (show coords) vis) mapped
  let result = maximumBy (compare `on` snd) $ mapped
  print $ snd result
  return (fst result)

normalize :: (Int, Int) -> (Int, Int)
normalize (x, y) =
  let g = gcd x y
  in (x `div` g, y `div` g)

groupByDirection :: (Int, Int) -> [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
groupByDirection from [] =
  Map.empty
groupByDirection from (p:ps)
  | p == from
  = groupByDirection from ps
  | otherwise
  = let d = p ^-^ from
        ed = normalize d
        m = Map.singleton ed [p]
        ms = groupByDirection from ps
    in Map.unionWith (++) m ms

toAngle :: (Int, Int) -> Double
toAngle (x, y) =
  atan2 (-dx) dy
  where
    dx = fromIntegral x
    dy = fromIntegral y

distanceFrom :: (Int, Int) -> (Int, Int) -> Double
distanceFrom (x1, y1) (x2, y2) =
  sqrt (dx * dx + dy * dy)
  where
    dx = fromIntegral (x2 - x1)
    dy = fromIntegral (y2 - y1)

laserPlan :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
laserPlan from xs =
  let grouped = groupByDirection from xs
      annotated =
        [ ( toAngle dir
          , dir
          , sort [ (distanceFrom from pos, pos) | pos <- items ]
          )
        | (dir, items)
        <- Map.toList grouped
        ]
      sorted = sort annotated
  in harvestPlan sorted

harvestPlan :: [ (Double, (Int, Int), [ (Double, (Int, Int)) ]) ] -> [(Int, Int)]
harvestPlan [] = []
harvestPlan items =
  let (thisStep, remainder) = unzip [ (pos, (angle, dir, xs)) | (angle, dir, (_, pos):xs) <- items ]
  in thisStep ++ harvestPlan remainder

part2 set from = do
  let plan = laserPlan from (Set.toList set)
  print $ plan !! 199

main = do
  rawInput <- lines <$> readFile "day10.input"
  let inputSet = inputToSet rawInput
      size = maximum (length rawInput : map length rawInput)
  from <- part1 size inputSet
  printf "FROM: %s\n" (show from)
  part2 inputSet from
