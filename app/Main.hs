module Main where

import Day01

main :: IO ()
main = do
  input <- getLine
  print (inverseCaptcha input)

