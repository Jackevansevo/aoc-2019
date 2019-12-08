module Day1
  ( fuel
  , part1
  , part2
  , totalFuel
  )
where

import Data.List (unfoldr)

fuel :: Int -> Int
fuel mass = (mass `quot` 3) - 2

fuelAmount :: Int -> Maybe (Int, Int)
fuelAmount b | b < 0 = Nothing
             | otherwise = Just (b, fuel b)

totalFuel :: Int -> Int
totalFuel mass = sum . tail $ unfoldr fuelAmount mass

part1 :: IO ()
part1 = do
  input <- readFile "inputs/day1part1.txt"
  print $ sum $ map (fuel . read :: String -> Int) $ lines input


part2 :: IO ()
part2 = do
  input <- readFile "inputs/day1part1.txt"
  print $ sum $ map (totalFuel . read :: String -> Int) $ lines input
