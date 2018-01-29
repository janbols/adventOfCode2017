{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (Left,Right,head,dropWhile,(!!))
import Data.Map
import Data.Stream hiding (fromList)
import Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestInput
  if ptNr == 1 then print $ pt1 $ read input
    else print $ pt2 $ read input

data Move = Left | Right | Up | Down deriving (Show)

type Position = (Int, Int)

type CellValue = Int

turn :: Move -> Move
turn Right = Up
turn Up = Left
turn Left = Down
turn Down = Right

type State = (Position, CellValue)
startState = ((0,0), 1) :: State

extractMove:: Position -> Move
extractMove (0,y) = if y > 0 then Left else Right
extractMove (x,0) = if x > 0 then Up else Down
extractMove (x,y) | x*y > 0 && x > 0 = if abs x > abs y then Up else Left
extractMove (x,y) | x*y < 0 && x < 0 = if abs x >= abs y then Down else Left
extractMove (x,y) | x*y > 0 && x < 0 = if abs x > abs y then Down else Right
extractMove (x,y) | x*y < 0 && x > 0 = if abs x > abs y then Up else Right

moveOne :: Position -> Position
moveOne pos@(x,y) = let move = extractMove pos
                   in case move of
                       Right -> (x + 1, y)
                       Up -> (x, y + 1)
                       Left -> (x - 1 , y)
                       Down -> (x, y - 1)

isStateLowerThan:: CellValue -> State -> Bool
isStateLowerThan target (_, val) = val < target

-- part 1
pt1 :: CellValue -> Int
pt1 target = let stream = unfold unfoldState startState :: Stream State
                 (pos,_) = head $ dropWhile (isStateLowerThan target) stream
             in manhattan pos

unfoldState:: State -> (State, State)
unfoldState (pos, val) = let newState = (moveOne pos, succ val)
                       in (newState, newState)

manhattan:: Position -> Int
manhattan (x,y) = abs x + abs y

--PART 2
type CellValueCalculator = Map Position CellValue

startCalculator = precalculatedValues (0,0) 1

precalculatedValues:: Position -> CellValue -> CellValueCalculator
precalculatedValues (x,y) val = fromList $ fmap (\t -> (t,val)) [(x',y')| x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]]

pt2 :: CellValue -> CellValue
pt2 target = let stream = unfold unfoldStateWithCalc (startState, startCalculator) :: Stream State
                 (_, val) =  head $ dropWhile (isStateLowerThan target) stream
             in val

unfoldStateWithCalc:: (State, CellValueCalculator) -> (State, (State, CellValueCalculator))
unfoldStateWithCalc ((pos, val), calc) = let nextPos = moveOne pos
                                             nextVal = calc ! nextPos
                                             nextState = (nextPos, nextVal)
                                             precalculatedVals = precalculatedValues nextPos nextVal
                                             nextCalc = unionWith (+) calc precalculatedVals
                                         in (nextState, (nextState, nextCalc))
