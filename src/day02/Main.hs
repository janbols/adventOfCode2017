{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
  _ <- putStrLn "Input..."
  interact (show . corruptionChecksumEvenDivide . parseMatrix)


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
