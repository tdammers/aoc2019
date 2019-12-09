{-#LANGUAGE FlexibleContexts #-}
module Main
where

import AOCUtils
import IntcodeVM
import Control.Monad
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.State

part1 :: VM -> IO ()
part1 vm = do
  (exitCode, vm', _) <- runVMWith (pure 1) (liftIO . print) run vm
  print exitCode

part2 :: VM -> IO ()
part2 vm = do
  (exitCode, vm', _) <- runVMWith (pure 5) (liftIO . print) run vm
  print exitCode

main = do
  program <- readInputInts "day5.input"
  let vm = loadVM program
  part1 vm
  part2 vm
