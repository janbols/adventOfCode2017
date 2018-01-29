{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.List
import Data.Maybe
import Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1 then print $ pt1 input
    else print $ pt2 input

pt1 :: String -> Int
pt1 = corruptionChecksumMinMax . parseMatrix

pt2 :: String -> Int
pt2 = corruptionChecksumEvenDivide . parseMatrix

parseMatrix :: String -> [[Int]]
parseMatrix = fmap parseRow . lines

parseRow :: String -> [Int]
parseRow = fmap read . words

--part 1
corruptionChecksumMinMax :: [[Int]] -> Int
corruptionChecksumMinMax = sum . fmap minMaxDiff

minMaxDiff :: [Int] -> Int
minMaxDiff xs = maximum xs - minimum xs

--part 2
corruptionChecksumEvenDivide :: [[Int]] -> Int
corruptionChecksumEvenDivide = sum . fmap evenDivide

isEvenlyDivided:: Int -> Int -> Bool
isEvenlyDivided x y = (x /= y) && (x `mod` y == 0)

evenDivide :: [Int] -> Int
evenDivide xs =
    let combinations = [(x,y) | x <- xs, y <-xs, x /= y]
    in case find (uncurry isEvenlyDivided) combinations of
      Just (x,y) -> max x y `div` min x y
      Nothing -> 0
