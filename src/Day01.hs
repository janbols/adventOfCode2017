module Day01 (inverseCaptcha) where

import Data.Char

inverseCaptcha :: String -> Int
inverseCaptcha cs = doInverseCaptcha (length cs `quot` 2) cs
--inverseCaptcha cs = doInverseCaptcha 1 cs


doInverseCaptcha :: Int -> String -> Int
doInverseCaptcha dropCnt cs =
  let xs = withPeek dropCnt (map digitToInt cs)
  in xs -: filter pairMatches -: map fst -: sum

withPeek:: Int -> [a]-> [(a,a)]
withPeek dropCnt xs = zip xs $ drop dropCnt $ cycle xs

pairMatches :: Eq a => (a,a) -> Bool
pairMatches = uncurry (==)

(-:) :: a -> ( a -> b ) -> b
x -: f = f x