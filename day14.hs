{-#LANGUAGE LambdaCase #-}
module Main
where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Data.Char
import Control.Monad
import Data.List
import Debug.Trace
import Text.Printf

parseInput :: [String] -> Map String (Int, [(Int, String)])
parseInput strs =
  foldl'
    Map.union
    Map.empty
    (map parseInputLine strs)

consumeWhile :: (Char -> Bool) -> State String String
consumeWhile p = state $ span p

consume :: String -> State String String
consume prefix = do
  str <- get
  when (not (prefix `isPrefixOf` str)) (error "BOOM")
  put $ drop (length prefix) str
  return prefix

parseInputLine :: String -> Map (String) (Int, [(Int, String)])
parseInputLine str =
  flip evalState str $ do
    inputs <- parseInputs
    void $ consumeWhile isSpace
    void $ consume "=>"
    void $ consumeWhile isSpace
    (outNum, outLabel) <- parsePair
    return $ Map.singleton outLabel (outNum, inputs)

parseInputs :: State String [(Int, String)]
parseInputs = do
  p <- parsePair
  void $ consumeWhile isSpace
  s <- get
  case s of
    ',':_ -> do
      void $ consume ","
      void $ consumeWhile isSpace
      t <- parseInputs
      return $ p:t
    '=':'>':_ -> do
      return [p]
    xs -> error $ "AAAARGH! " ++ show xs

parsePair :: State String (Int, String)
parsePair = do
  num <- read <$> consumeWhile isDigit
  void $ consumeWhile isSpace
  label <- consumeWhile isAlpha
  void $ consumeWhile isSpace
  return (num, label)

part1 :: Map String (Int, [(Int, String)]) -> IO Int
part1 defs = do
  let solution = runFactory defs Map.empty [(1, "FUEL")]
  print solution
  return solution

runFactory :: Map String (Int, [(Int, String)]) -> Map String Int -> [(Int, String)] -> Int
runFactory defs _ [] = 0
runFactory defs leftovers ((n, "ORE"):xs) =
  n + runFactory defs leftovers xs
runFactory defs leftovers ((n, label):xs) =
  let (fromLeftovers, leftovers') =
          case Map.lookup label leftovers of
            Nothing -> (0, leftovers)
            Just x ->
              let x' = min x n
              in (x', Map.insert label (x - x') leftovers)
      neededFromRuns = n - fromLeftovers
      Just (runOutput, runInputs) = Map.lookup label defs
      numRuns = (neededFromRuns + runOutput - 1) `div` runOutput
      fromRuns = runOutput * numRuns
      needs' = xs ++ [(amount * numRuns, l) | (amount, l) <- runInputs]
      leftovers'' = Map.insertWith (+) label (fromRuns - neededFromRuns) leftovers'
  in runFactory defs leftovers'' needs'

oneTrillion = 1000000000000

part2 :: Int -> Int -> Map String (Int, [(Int, String)]) -> IO ()
part2 minb maxb defs
  = do
      let minOre = runFactory defs Map.empty [(minb, "FUEL")]
          maxOre = runFactory defs Map.empty [(maxb, "FUEL")]
      when (maxOre < oneTrillion) (error "Upper bound too low")
      when (minOre > oneTrillion) (error "Lower bound too high")
      solution <- go minb minOre maxb maxOre
      print solution
  where
    go :: Int -> Int -> Int -> Int -> IO Int
    go minb minOre maxb maxOre
      | minb == maxb
      = return minb
      | minb + 1 == maxb
      = return $ if maxOre <= oneTrillion then maxb else minb
      | otherwise
      = do
          let midb = (minb + maxb) `div` 2
              midOre = runFactory defs Map.empty [(midb, "FUEL")]
          case compare midOre oneTrillion of
            EQ -> return midb
            LT -> go midb midOre maxb maxOre
            GT -> go minb minOre midb midOre

main = do
  input <- parseInput . lines <$> readFile "day14.input"
  solution1 <- part1 input
  part2 solution1 (solution1 * 1000) input
