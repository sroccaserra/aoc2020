module Day08 where

import qualified Data.Set as S

main = interact $ show . partOne . (map parseLine) . lines
-- main = do
--   xs <- fmap lines getContents
--   mapM_ print $ map parseLine xs

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

partOne xs = execPrg S.empty $ Machine (cycle xs) 0 0

type Instruction = (Operation, Int)
data Operation = Jmp | Acc | Nop
               deriving (Show)

data Machine = Machine [Instruction] Int Int

instance Show Machine where
    show (Machine prg pc acc) = show (pc, acc, take 10 prg)

execPrg s m@(Machine _ pc _) | S.member pc s = m
execPrg s m@(Machine prg pc _) = execPrg (S.insert pc s) $ execInstruction m op n
  where (op, n) = prg !! pc

execInstruction :: Machine -> Operation -> Int -> Machine
execInstruction (Machine prg pc acc) Acc n = (Machine prg (succ pc) $ acc + n)
execInstruction (Machine prg pc acc) Jmp n = (Machine prg (pc + n) acc)
execInstruction (Machine prg pc acc) Nop _ = (Machine prg (succ pc) acc)

findInstruction (Machine prg pc _) = prg !! pc
