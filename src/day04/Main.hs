{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Utils
import Data.List

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1 then print $ pt1 $ lines input
    else print $ pt2 $ lines input

-- part 1
pt1 :: [String] -> Int
pt1 ls = length $ filter (not . hasDuplicate) $ map words ls

hasDuplicate:: [String] -> Bool
hasDuplicate [] = False
hasDuplicate (w:ws) = w `elem` ws || hasDuplicate ws


-- part 2
pt2 :: [String] -> Int
pt2 ls = length $ filter (not . hasAnagram) $ map words ls

hasAnagram:: [String] -> Bool
hasAnagram ws = hasDuplicate $ map sort ws
