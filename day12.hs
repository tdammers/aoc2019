{-#LANGUAGE OverloadedLists #-}
module Main
where

import AOCUtils
import Data.List
import Debug.Trace
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Moon =
  Moon
    { moonPos :: !(Int, Int, Int)
    , moonVel :: !(Int, Int, Int)
    }

instance Show Moon where
  show (Moon pos vel) = show pos ++ show vel

defMoon :: Moon
defMoon = Moon (0,0,0) (0,0,0)

applyGravity :: Vector Moon -> Vector Moon
applyGravity moons = do
  this <- moons
  let gravity = Vector.foldl' (^+^) (0,0,0) $ do
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
potentialMoonEnergy Moon { moonPos = (x,y,z) } = abs x + abs y + abs z

kineticMoonEnergy :: Moon -> Int
kineticMoonEnergy Moon { moonVel = (x,y,z) } = abs x + abs y + abs z

totalMoonEnergy :: Moon -> Int
totalMoonEnergy moon =
  potentialMoonEnergy moon * kineticMoonEnergy moon

totalEnergy :: Vector Moon -> Int
totalEnergy = Vector.sum . Vector.map totalMoonEnergy

step :: Vector Moon -> Vector Moon
step moons =
  trace (take 80 . show $ moons) $
  applyVelocities . applyGravity $ moons

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x =
  let x' = applyN (n - 1) f x
  in x' `seq` f x'

part1 = do
  let moons :: Vector Moon
      moons =
        [ defMoon { moonPos = ( 6, -2, -7) }
        , defMoon { moonPos = (-6, -7, -4) }
        , defMoon { moonPos = (-9, 11, 0) }
        , defMoon { moonPos = (-3, -4, 6) }
        ]
  run1 moons 1000

example1 = do
  let moons :: Vector Moon
      moons =
        [ defMoon { moonPos = (-1,   0, 2 ) }
        , defMoon { moonPos = ( 2, -10, -7) }
        , defMoon { moonPos = ( 4,  -8, 8 ) }
        , defMoon { moonPos = ( 3,   5, -1) }
        ]
  run1 moons 10

run1 moons steps = do
  let moons' = applyN steps step moons
  print moons'
  print $ totalEnergy moons'

main = part1
