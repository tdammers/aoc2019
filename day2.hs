{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Control.Monad.State
import Text.Printf
import Data.Maybe
import Debug.Trace
import AOCUtils

import IntcodeVM

part1 :: VM -> IO ()
part1 vm = do
  (exitCond, vm', _) <- flip runVM vm $ do
    poke 1 12
    poke 2 2
    run
  print exitCond
  dumpVM vm'

part2 :: VM -> IO ()
part2 vm = do
  putStrLn "Brute-forcing..."
  forM_ [0..100] $ \n -> do
    forM_ [0..100] $ \v -> do
      (exitCond, vm', _) <- flip runVM vm $ do
        poke 1 n
        poke 2 v
        run
      let result = IntMap.lookup 0 . vmRAM $ vm'
      when (result == Just 19690720) $ do
        printf "Solution: %d %d -> %d\n" n v (n * 100 + v)

main = do
  program <- readInputInts "day2.input"
  let vm = loadVM program
  part1 vm
  part2 vm
