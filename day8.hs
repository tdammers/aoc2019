{-#LANGUAGE LambdaCase #-}
module Main
where

import AOCUtils
import Data.List
import Data.Function
import Data.Char
import Control.Monad

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs =
  let (x, r) = splitAt n xs
  in x : chunks n r

part1 :: [Int] -> IO ()
part1 rawData =
  let layers = chunks (25 * 6) rawData
      interestingLayer = minimumBy (compare `on` (length . filter (== 0))) layers
  in print $
       length (filter (== 1) interestingLayer) *
       length (filter (== 2) interestingLayer)

foldLayers :: [[Int]] -> [Int]
foldLayers = foldl' mergeLayers (repeat 2)

mergeLayers :: [Int] -> [Int] -> [Int]
mergeLayers = zipWith mergePixels

mergePixels :: Int -> Int -> Int
mergePixels 2 x = x
mergePixels x _ = x

renderLayer :: Int -> [Int] -> IO ()
renderLayer w pixels = do
  putStrLn $ replicate w '-'
  let rows = chunks w pixels
  forM_ rows $ \row -> do
    forM_ row $ \case
      0 -> putStr " "
      1 -> putStr "X"
      x -> putStr (show x)
    putStrLn ""
  putStrLn $ replicate w '-'

part2 :: [Int] -> IO ()
part2 rawData = do
  let layers = chunks (25 * 6) rawData
  renderLayer 25 . foldLayers $ layers

main = do
  imageData <- map (read . (:[])) . filter isDigit <$> readFile "day8.input"
  part1 imageData
  part2 imageData
