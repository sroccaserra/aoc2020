module Day08 where

import Data.Array
import qualified Data.Set as S

main = interact $ show . partTwo . (map parseLine) . lines

parseLine :: String -> Instruction
parseLine = parseInstructions . words
  where parseInstructions (x:y:_) = (readOp x, read $ map clean y)
        parseInstructions ws = error $ "Invalid line: " ++ (unwords ws)
        clean '+' = ' '
        clean c = c
        readOp "acc" = Acc
        readOp "jmp" = Jmp
        readOp "nop" = Nop
        readOp i = error $ "Invalid instruction: " ++ i

partOne xs = execPrg S.empty $ Machine prg 0 0
  where prg = listArray (0, pred $ length xs) xs

partTwo xs = findHaltingPrg prg 0 $ Machine prg 0 0
  where prg = listArray (0, pred $ length xs) xs

type Instruction = (Operation, Int)
data Operation = Jmp | Acc | Nop
               deriving (Show)

data Machine = Machine (Array Int Instruction) Int Int

instance Show Machine where
    show (Machine prg pc acc) = show (pc, acc, prg!pc)

execPrg :: S.Set Int -> Machine -> Machine
execPrg s m | isLooping s m = m
execPrg _ m | hasEnded m = m
execPrg s m@(Machine prg pc _) = execPrg (S.insert pc s) $ execInstruction m op v
  where (op, v) = prg ! pc

execInstruction :: Machine -> Operation -> Int -> Machine
execInstruction (Machine prg pc acc) Acc n = Machine prg (succ pc) $ acc + n
execInstruction (Machine prg pc acc) Jmp n = Machine prg (pc + n) acc
execInstruction (Machine prg pc acc) Nop _ = Machine prg (succ pc) acc

findHaltingPrg :: Array Int Instruction -> Int -> Machine -> Machine
findHaltingPrg _ _ m | hasEnded m = m
findHaltingPrg prg n _ = findHaltingPrg prg (n+1) $ execPrg S.empty $ (mute (n+1) $ Machine prg 0 0)

hasEnded (Machine prg pc _) = snd (bounds prg) == pc

isLooping s (Machine _ pc _) = S.member pc s

mute :: Int -> Machine -> Machine
mute n (Machine prg pc acc) = Machine (hack n prg) pc acc
  where hack i prg = prg // [(i, switch $ prg!i)]
        switch (Nop, n) = (Jmp, n)
        switch (Jmp, n) = (Nop, n)
        switch (o, n) = (o, n)
