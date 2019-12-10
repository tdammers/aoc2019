module Main
where

import IntcodeVM
import AOCUtils
import Data.IORef
import Data.List
import Control.Monad.IO.Class
import Text.Printf

runAmpChain :: VM -> [Int] -> IO Int
runAmpChain vm phases =
  go 0 phases
  where
    go input [] = pure input
    go input (x:xs) = do
      inputsVar <- newIORef [x, input]
      outputVar <- newIORef Nothing
      (exitCode, vm', _) <- runVMWith 
        (liftIO $ do
          (i:is) <- readIORef inputsVar
          writeIORef inputsVar is
          return i)
        (liftIO . writeIORef outputVar . Just)
        run
        vm
      Just output <- readIORef outputVar
      go output xs

part1 vm = do
  print =<< maximum <$> mapM (runAmpChain vm) (permutations [0..4])

main = do
  program <- readInputInts "day7.input"
  let vm = loadVM program
  part1 vm
