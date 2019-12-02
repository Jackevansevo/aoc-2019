module Day1
  ( fuel
  , allFuels
  , totalFuel
  , day1
  )
where

fuel mass = (mass `quot` 3) - 2

totalFuel mass = sum $ allFuels [mass]

allFuels :: [Int] -> [Int]
allFuels all@(mass : _) | req < 1   = init all
                        | otherwise = allFuels (req : all)
  where req = (fuel mass)

day1part1 :: IO ()
day1part1 = do
  input <- readFile "inputs/day1part1.txt"
  print $ sum $ map (totalFuel . read :: String -> Int) $ lines input


day2part2 :: IO ()
day2part2 = do
  input <- readFile "inputs/day1part1.txt"
  print $ sum $ map (totalFuel . read :: String -> Int) $ lines input
