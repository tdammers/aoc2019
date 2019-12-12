{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Text.Printf
import Data.Maybe
import Debug.Trace
import AOCUtils
import Data.IORef

import IntcodeVM

data Direction
  = DirUp
  | DirRight
  | DirDown
  | DirLeft
  deriving (Show, Eq, Ord, Enum, Bounded)

nextDir :: Direction -> Direction
nextDir DirUp = DirRight
nextDir DirRight = DirDown
nextDir DirDown = DirLeft
nextDir DirLeft = DirUp

prevDir :: Direction -> Direction
prevDir DirRight = DirUp
prevDir DirDown = DirRight
prevDir DirLeft = DirDown
prevDir DirUp = DirLeft

dirVec :: Direction -> (Int, Int)
dirVec DirUp = (0, -1)
dirVec DirRight = (1, 0)
dirVec DirDown = (0, 1)
dirVec DirLeft = (-1, 0)

data RobotState =
  RobotState
    { robotPosition :: (Int, Int)
    , robotDirection :: Direction
    , robotCommandBuf :: Maybe Int
    , robotSquaresPainted :: Map (Int, Int) Int
    }
    deriving (Show)

defRobotState :: RobotState
defRobotState = RobotState (0, 0) DirUp Nothing Map.empty

robotCommand :: Int -> RobotState -> RobotState
robotCommand cmd rs@RobotState { robotCommandBuf = Nothing } =
  rs { robotCommandBuf = Just cmd }
robotCommand cmd1 rs@RobotState { robotCommandBuf = Just cmd0 } =
  rs { robotCommandBuf = Nothing
     , robotDirection = robotDirection'
     , robotSquaresPainted = robotSquaresPainted'
     , robotPosition = robotPosition'
     }
  where
    robotDirection' =
        (if cmd1 == 0 then prevDir else nextDir)
          (robotDirection rs)
    robotSquaresPainted' =
      Map.insert (robotPosition rs) cmd0 (robotSquaresPainted rs)
    robotPosition' =
      robotPosition rs ^+^ dirVec robotDirection'

robotRead :: RobotState -> Int
robotRead rs =
  fromMaybe 0 $ Map.lookup (robotPosition rs) (robotSquaresPainted rs)

part1 :: VM -> IO ()
part1 vm = do
  robotVar <- newIORef defRobotState
  (exitCond, vm', _) <-
      runVMWith
        (liftIO $ robotRead <$> readIORef robotVar)
        (\cmd -> liftIO $ modifyIORef robotVar (robotCommand cmd))
        run vm
  readIORef robotVar >>= print . Map.size . robotSquaresPainted
  print exitCond

part2 :: VM -> IO ()
part2 vm = do
  robotVar <- newIORef defRobotState { robotSquaresPainted = Map.singleton (0,0) 1 }
  (exitCond, vm', _) <-
      runVMWith
        (liftIO $ robotRead <$> readIORef robotVar)
        (\cmd -> liftIO $ modifyIORef robotVar (robotCommand cmd))
        run vm
  readIORef robotVar >>= drawSquares . robotSquaresPainted
  print exitCond

drawSquares :: Map (Int, Int) Int -> IO ()
drawSquares painted =
  forM_ [t..b] $ \y -> do
    forM_ [l..r] $ \x -> do
      case Map.lookup (x,y) painted of
        Just 1 -> putStr "#"
        _ -> putStr "."
    putStrLn ""
  where
    keys = Map.keys painted
    l = minimum . map fst $ keys
    r = maximum . map fst $ keys
    t = minimum . map snd $ keys
    b = maximum . map snd $ keys

main = do
  program <- readInputInts "day11.input"
  let vm = loadVM program
  part1 vm
  part2 vm
