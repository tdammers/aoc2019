module Main
where

import IntcodeVM
import AOCUtils
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Text.Printf

runAmpChain :: VM -> [Int] -> IO Int
runAmpChain vm [] =
  pure 0
runAmpChain vm phases = do
  print phases
  inputVars@(inputVar0:inputVarsN) <- forM phases newMVar
  let outputVars = inputVarsN ++ [inputVar0]
  (_, results) <- concurrently
    (putMVar inputVar0 0)
    (forConcurrently (zip inputVars outputVars) $ \(inputVar, outputVar) -> do
      runVMWith (liftIO $ takeMVar inputVar) (liftIO . putMVar outputVar) run vm
    )
  forM_ (zip [0..] results) $ \(i, (exitCode, vm', _)) -> do
    printf "%d: %s\n" (i :: Int) (show exitCode)
  readMVar inputVar0

part1 vm = do
  print =<< maximum <$> mapM (runAmpChain vm) (permutations [0..4])

part2 vm = do
  print =<< maximum <$> mapM (runAmpChain vm) (permutations [5..9])

main = do
  program <- readInputInts "day7.input"
  let vm = loadVM program
  part1 vm
  part2 vm
