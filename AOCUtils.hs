{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
module AOCUtils
where

import Data.List
import Data.Char
import Data.Maybe

keepDigit :: Char -> Char
keepDigit c
  | isDigit c = c
  | otherwise = ' '

readInputInts :: FilePath -> IO [Int]
readInputInts =
  readInputFile (fmap (fmap (dropWhile (not . (`elem` "0123456789-")))) <$> listToMaybe . reads @Int)

parseInput :: (String -> Maybe (a, String)) -> String -> ([a], String)
parseInput p "" = ([], "")
parseInput p str =
  case p str of
    Nothing ->
      ([], str)
    Just (item, str') ->
      let (items, str'') = parseInput p str'
      in (item:items, str'')

readInputFile :: (String -> Maybe (a, String)) -> FilePath -> IO [a]
readInputFile p fn =
  (parseInput p <$> readFile fn) >>= \case
    (items, "") -> return items
    (_, rem) -> error $ "Unexpected: " ++ show rem

class Num a => Vec f a | f -> a where
  (^+^) :: Num a => f -> f -> f
  (^-^) :: Num a => f -> f -> f

  componentWise :: (a -> a) -> f -> f
  componentWise2 :: (a -> a -> a) -> f -> f -> f

  (^*) :: f -> a -> f
  (*^) :: a -> f -> f
  (^+^) = componentWise2 (+)
  (^-^) = componentWise2 (+)
  (*^) = \s x -> componentWise (s *) x
  (^*) = \x s -> componentWise (* s) x

instance Num a => Vec (a,a) a where
  componentWise f (x, y) =
    (f x, f y)
  componentWise2 f (x1, y1) (x2, y2) =
    (f x1 x2, f y1 y2)

instance Num a => Vec (a,a,a) a where
  componentWise f (x, y, z) =
    (f x, f y, f z)
  componentWise2 f (x1, y1, z1) (x2, y2, z2) =
    (f x1 x2, f y1 y2, f z1 z2)
