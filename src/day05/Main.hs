{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (readList,length)
import Utils
import Data.Sequence
import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1 then print $ pt1 $ fromList $ readList input
    else print $ pt2 $ fromList $ readList input

type Index = Int
type Step = Int
type Offset = Int
type OffsetStrategy = Offset -> Offset
type Updater = Index -> Seq Offset -> (Index, Seq Offset)

readList:: Read a => String -> [a]
readList input = read <$> lines input

jumpSteps :: Updater -> Seq Offset -> Step
jumpSteps updater ls =
  let (_,step) = evalState (seqState updater (0,0)) ls
  in step

seqState:: Updater -> (Index,Step) -> State (Seq Offset) (Index,Step)
seqState updater (currentIx,currentStep) =
  do seq <- get
     let (newIx,newSeq) = updater currentIx seq
     put newSeq
     if newIx >= length newSeq then return (newIx,currentStep+1)
       else seqState updater (newIx,currentStep+1)


updateSeq :: OffsetStrategy -> Updater
updateSeq offsetStrategy currentIx seq =
    let jump = index seq currentIx
        newSeq = adjust offsetStrategy currentIx seq
    in (currentIx + jump, newSeq)



-- part 1
pt1 :: Seq Offset -> Step
pt1 = jumpSteps $ updateSeq succ

-- part 1
pt2 :: Seq Offset -> Step
pt2 = jumpSteps $ updateSeq (\o -> if o >= 3 then o-1 else o+1)
