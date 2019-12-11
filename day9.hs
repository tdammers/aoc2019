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
  (exitCond, vm', _) <- runVMWith (return 1) (liftIO . print) run vm
  print exitCond

part2 :: VM -> IO ()
part2 vm = do
  (exitCond, vm', _) <- runVMWith (return 2) (liftIO . print) run vm
  print exitCond

main = do
  program <- readInputInts "day9.input"
  let vm = loadVM program
  part1 vm
  part2 vm
