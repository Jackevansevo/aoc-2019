module Day8 where

import           Data.List.Split
import           Data.List
import           Data.Char

rstrip = reverse . dropWhile isSpace . reverse

count x = length . filter (x ==)

part1 :: IO ()
part1 = do
  input <- readFile "inputs/day8.txt"
  let w          = 25
      h          = 6
      layers     = chunksOf (w * h) $ map digitToInt $ rstrip input
      leastZeros = head $ sortOn (count 0) layers
  print $ count 1 leastZeros * count 2 leastZeros

pixelColour :: [Int] -> Int
pixelColour (x : xs) | x == 0 = x
                     | x == 1 = x
                     | x == 2 = pixelColour xs

toChar :: Int -> String
toChar x | x == 1    = "#"
         | otherwise = " "

toScreen :: [Int] -> String
toScreen x = intercalate "" $ map toChar x

part2 :: IO ()
part2 = do
  input <- readFile "inputs/day8.txt"
  let w       = 25
      h       = 6
      layers  = chunksOf (w * h) $ map digitToInt $ rstrip input
      subLayers    = concat $ map (chunksOf h . chunksOf w) layers
      columns = concat . map transpose $ transpose subLayers
      colours = map pixelColour columns
      pixels  = concat . chunksOf h . chunksOf w $ colours
  putStr $ intercalate "\n" $ map toScreen $ pixels
