module Day2 ( processOpcode
  , readInstruction
  , Instruction
  , day2part1
  , day2part2
  , initializeProgram
  , runProgram
  )
where

import           Data.List.Split

data Instruction = Instruction {op :: Int, x :: Int, y :: Int, loc :: Int} deriving (Show)

data OpResult = Value Int | Halt | Error deriving (Show)


-- TODO: Oh dear this clearly isn't how you're supposed to Haskell

replaceAtIndex :: Int -> Int -> [Int] -> [Int]
replaceAtIndex n item ls = a ++ (item : b) where (a, (_ : b)) = splitAt n ls

runProgram :: [Int] -> Int -> Int
runProgram inputs pos = do
  case result of
    Value v -> runProgram (replaceAtIndex (loc instruction) v inputs) (pos + 4)
    Halt    -> head inputs
    Error   -> error $ show inputs
 where
  instruction = readInstruction inputs pos
  xVal        = inputs !! (x instruction)
  yVal        = inputs !! (y instruction)
  result      = processOpcode (op instruction) xVal yVal


makeInstruction :: [Int] -> Instruction
makeInstruction (a : b : c : d : []) = Instruction a b c d

readInstruction :: [Int] -> Int -> Instruction
readInstruction inputs pos = makeInstruction $ take 4 $ drop pos $ inputs


processOpcode :: Int -> Int -> Int -> OpResult
processOpcode op x y | op == 1   = Value (x + y)
                     | op == 2   = Value (x * y)
                     | op == 99  = Halt
                     | otherwise = Error


initializeProgram :: [Int] -> Int -> Int -> [Int]
initializeProgram (x:_:_:xs) a b = x : [a, b] ++ xs

day2part1 :: IO ()
day2part1 = do
  input <- readFile "inputs/day2part1.txt"
  let vals = (map (read :: String -> Int) $ splitOn "," input)
      mem  = initializeProgram vals 12 2
      -- TODO: Christ
  print $ [x | x <- [if (runProgram (initializeProgram vals x y) 0) == 19690720 then (x,y) else (0,0) | x <- [0..99], y <- [0..99]], x /= (0,0)]


day2part2 :: IO ()
day2part2 = do
  input <- readFile "inputs/day2part1.txt"
  let vals = (map (read :: String -> Int) $ splitOn "," input)
  print $ runProgram vals 0
