{-#LANGUAGE OverloadedLists #-}
module Main
where

import AOCUtils
import Data.List
import Debug.Trace
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Applicative
import Control.Monad
import Data.Maybe
import Text.Printf
import System.IO

data Moon =
  Moon
    { moonPos :: !(Vector Int)
    , moonVel :: !(Vector Int)
    }
    deriving (Eq)

instance Show Moon where
  show (Moon pos vel) = show pos ++ show vel

defMoon :: Moon
defMoon = Moon [0,0,0] [0,0,0]

applyGravity :: Vector Moon -> Vector Moon
applyGravity moons = do
  this <- moons
  let gravity = Vector.foldl' (^+^) [0,0,0] $ do
        that <- moons
        return $ componentWise2 gravity1d (moonPos this) (moonPos that)
  return this { moonVel = moonVel this ^+^ gravity }
  where
    gravity1d a b =
      case compare a b of
        LT -> 1
        EQ -> 0
        GT -> -1

applyVelocities :: Vector Moon -> Vector Moon
applyVelocities moons = do
  this <- moons
  return this { moonPos = moonPos this ^+^ moonVel this }

potentialMoonEnergy :: Moon -> Int
potentialMoonEnergy Moon { moonPos = pos } = Vector.sum . Vector.map abs $ pos

kineticMoonEnergy :: Moon -> Int
kineticMoonEnergy Moon { moonVel = vel } = Vector.sum . Vector.map abs $ vel

totalMoonEnergy :: Moon -> Int
totalMoonEnergy moon =
  potentialMoonEnergy moon * kineticMoonEnergy moon

totalEnergy :: Vector Moon -> Int
totalEnergy = Vector.sum . Vector.map totalMoonEnergy

step :: Vector Moon -> Vector Moon
step moons =
  applyVelocities . applyGravity $ moons

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x =
  let x' = applyN (n - 1) f x
  in x' `seq` f x'

inputMoons :: Vector Moon
inputMoons =
  [ defMoon { moonPos = [ 6, -2, -7] }
  , defMoon { moonPos = [-6, -7, -4] }
  , defMoon { moonPos = [-9, 11, 0] }
  , defMoon { moonPos = [-3, -4, 6] }
  ]

exampleMoons :: Vector Moon
exampleMoons =
  [ defMoon { moonPos = [-1,   0, 2 ] }
  , defMoon { moonPos = [ 2, -10, -7] }
  , defMoon { moonPos = [ 4,  -8, 8 ] }
  , defMoon { moonPos = [ 3,   5, -1] }
  ]

exampleMoonsB :: Vector Moon
exampleMoonsB =
  [ defMoon { moonPos = [-8, -10,  0] }
  , defMoon { moonPos = [ 5,   5, 10] }
  , defMoon { moonPos = [ 2,  -7,  3] }
  , defMoon { moonPos = [ 9,  -8, -3] }
  ]

example1 = do
  run1 exampleMoons 10

part1 = do
  run1 inputMoons 1000

run1 moons steps = do
  let moons' = applyN steps step moons
  print moons'
  print $ totalEnergy moons'

part2 = do
  run2 inputMoons

run2 :: Vector Moon -> IO ()
run2 moons = do
  let xs = collect 200000 moons
      dim = Vector.length (Vector.head $ Vector.head xs)
      cycles = map (detectCycle xs) [0..dim-1]
  print dim
  mapM_ print xs
  print $ map fromJust cycles
  print (foldl lcm 1 $ map fromJust cycles)

detectCycle :: Vector (Vector (Vector Int)) -> Int -> Maybe Int
detectCycle xs n =
  (* 2) . succ <$> Vector.findIndex (Vector.all (== 0) . Vector.map (Vector.! n)) (Vector.tail xs)

collect :: Int -> Vector Moon -> Vector (Vector (Vector Int))
collect 0 moons =
  []
collect n moons =
  let cur :: Vector (Vector Int)
      cur = Vector.map moonVel moons
      t = collect (pred n) (step moons)
  in Vector.cons cur t

main = do
  -- part1
  part2
