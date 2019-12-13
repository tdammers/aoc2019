{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State
import Text.Printf
import Data.Maybe
import Debug.Trace
import AOCUtils
import Data.IORef
import Data.List
import Control.Concurrent (threadDelay)
import System.IO

import IntcodeVM

part1 :: VM -> IO ()
part1 vm = do
  pipelineVar <- newIORef []
  (exitCond, vm', _) <-
      runVMWith
        (pure 0)
        (\b -> liftIO $ modifyIORef pipelineVar (b:))
        run vm
  pipeline <- chunks 3 . reverse <$> readIORef pipelineVar
  let screen :: Map (Int, Int) Int
      screen = foldl' (flip $ uncurry Map.insert) Map.empty [ ((x,y), t) | [x,y,t] <- pipeline ]
  print exitCond
  drawSquares screen
  print $ length . filter (== 2) . Map.elems $ screen

part2 :: VM -> IO ()
part2 vm = do
  paddleVar <- newIORef 0
  ballVar <- newIORef 0
  scoreVar <- newIORef 0
  commandVar <- newIORef []
  blocksVar <- newIORef Set.empty
  screenVar <- newIORef Map.empty

  -- calculate joystick position
  let inputFunc = do
        threadDelay 100000
        paddle <- readIORef paddleVar
        ball <- readIORef ballVar
        pure $ signum (ball - paddle)

  let outputFunc b = do
        cmd <- readIORef commandVar
        let cmd' = b:cmd
        case cmd' of
          [score,0,-1] -> do
            writeIORef scoreVar score
            putStr "\ESC[1;1H"
            readIORef scoreVar >>= printf "SCORE: %8d\n"
            writeIORef commandVar []
          [t,y,x] -> do
            -- handle blocks
            if t == 2 then
              modifyIORef blocksVar (Set.insert (x,y))
            else
              modifyIORef blocksVar (Set.delete (x,y))

            case t of
              3 -> writeIORef paddleVar x
              4 -> writeIORef ballVar x
              _ -> pure ()
            modifyIORef screenVar (Map.insert (x,y) t)
            printf "\ESC[%d;%dH" (y+2) (x * 2 + 1)
            drawSquare t
            printf "\ESC[30;1H"
            hFlush stdout
            writeIORef commandVar []
          _ -> do
            writeIORef commandVar cmd'

  putStrLn "\ESC[2J"

  (exitCond, vm', _) <-
      runVMWith
        (liftIO inputFunc)
        (liftIO . outputFunc)
        (do
          poke 0 2
          run
        )
        vm
  print exitCond

drawSquare :: Int -> IO ()
drawSquare = \case
  0 -> putStr "  " -- empty
  1 -> putStr "##" -- wall
  2 -> putStr "[]" -- block
  3 -> putStr "==" -- paddle
  4 -> putStr "()" -- ball

drawSquares :: Map (Int, Int) Int -> IO ()
drawSquares painted =
  forM_ [t..b] $ \y -> do
    forM_ [l..r] $ \x -> do
      drawSquare . fromMaybe 0 $ Map.lookup (x,y) painted
  where
    keys = Map.keys painted
    l = minimum . map fst $ keys
    r = maximum . map fst $ keys
    t = minimum . map snd $ keys
    b = maximum . map snd $ keys

main = do
  program <- readInputInts "day13.input"
  let vm = loadVM program
  part2 vm
