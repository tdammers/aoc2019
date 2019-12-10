module Main
where

import Data.Map (Map)
import qualified Data.Map as Map

-- countOrbits :: Map String String -> Int
countOrbits orbitParents =
  sum $ map countOrbitsFrom keys
  where
    keys = Map.keys orbitParents
    countOrbitsFrom k =
      case Map.lookup k orbitParents of
        Nothing -> 0
        Just p -> succ $ countOrbitsFrom p

trackOrbits orbitParents k =
  reverse . drop 1 $ go k
  where
    go k =
      case Map.lookup k orbitParents of
        Nothing -> [k]
        Just p -> k:go p

parseOrbit :: String -> (String, String)
parseOrbit src =
  let (l, r) = break (== ')') src
  in (drop 1 r, l)

dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
dropCommonPrefix (x:xs) (y:ys)
  | x == y
  = let (p, l, r) = dropCommonPrefix xs ys
    in (x:p, l, r)
dropCommonPrefix xs ys
  = ([], xs, ys)

part1 :: Map String String -> IO ()
part1 orbits =
  print $ countOrbits orbits

part2 :: Map String String -> IO ()
part2 orbits = do
  let pSanta = trackOrbits orbits "SAN"
      pYou = trackOrbits orbits "YOU"
  let (prefix, santa, you) = dropCommonPrefix pSanta pYou
  print $ length santa + length you

main = do
  orbits <- Map.fromList . map parseOrbit . lines <$> readFile "day6.input"
  part1 orbits
  part2 orbits
