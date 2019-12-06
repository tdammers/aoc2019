module AOCUtils
where

import Data.List
import Data.Char

keepDigit :: Char -> Char
keepDigit c
  | isDigit c = c
  | otherwise = ' '

readInputInts :: FilePath -> [Int]
readInputInts fn = map read . words . map keepDigit <$> readFile fn
