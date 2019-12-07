module Main
where

import Data.List

candidate1 :: Int -> Bool
candidate1 c =
  hasAdjacentDigits && digitsIncreasing
  where
    s = show c
    hasAdjacentDigits = any (\xs -> length xs >= 2) (group s)
    digitsIncreasing = not $ any id (zipWith (>) s (tail s))

candidate2 :: Int -> Bool
candidate2 c =
  hasAdjacentDigits && digitsIncreasing
  where
    s = show c
    hasAdjacentDigits = any (\xs -> length xs == 2) (group s)
    digitsIncreasing = not $ any id (zipWith (>) s (tail s))

main = do
  let inputRange = [235741..706948]
  -- part 1
  print . length . filter candidate1 $ inputRange
  -- part 2
  print . length . filter candidate2 $ inputRange
