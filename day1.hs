module Main where

fuelForMass :: Integer -> Integer
fuelForMass m =
  max 0 $ m `div` 3 - 2

fuelForModule :: Integer -> Integer
fuelForModule m =
  case fuelForMass m of
    0 -> 0
    f -> f + fuelForModule f

fuelForModules :: [Integer] -> Integer
fuelForModules = sum . map fuelForModule

main = do
  modules <- map read . lines <$> readFile "day1.input"
  print . sum . map fuelForMass $ modules
  print . sum . map fuelForModule $ modules
