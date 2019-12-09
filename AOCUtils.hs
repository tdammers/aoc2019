{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TypeApplications #-}
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
