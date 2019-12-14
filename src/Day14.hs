module Day14 where

import Data.List (groupBy)
import Data.Function
import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as T

strip = T.unpack . T.strip . T.pack

-- Need to perform substition for each instruction, reducing instructions down
-- until an Ore or Fuel value is obtained. Hopefully grouping all the equations
-- at every stage should work
--
-- Start by only reducing instructions where there's an exact recipe (or a
-- recipe with a remainder), else just keep expanding and reducing results
-- until the output doesn't change
--
-- 10 ORE => 10 A
-- 1 ORE => 1 B
-- 7 A, 1 B => 1 C
-- 7 A, 1 C => 1 D
-- 7 A, 1 D => 1 E
-- 7 A, 1 E => 1 FUEL
--
-- 1 FUEL => 7 A + 1 E
-- 1 FUEL => 7 A + (7 A + 1 D)
-- 1 FUEL => 7 A + (7 A + (7 A + 1 C))
-- 1 FUEL => 7 A + (7 A + (7 A + (7 A + 1 B))
-- 1 FUEL => 7 A + (7 A + (7 A + (7 A + (1 ORE)))
-- 1 FUEL => 7 A + 7 A + 7 A + 7 A + 1 ORE
-- 1 FUEL => 28 A + 1 ORE
-- 1 FUEL => (CEIL 28/10 * 10) + 1 ORE
-- 1 FUEL => 30 ORE + 1 ORE
-- 1 FUEL => 31 ORE


-- 9 ORE => 2 A
-- 8 ORE => 3 B
-- 7 ORE => 5 C
-- 3 A, 4 B => 1 AB
-- 5 B, 7 C => 1 BC
-- 4 C, 1 A => 1 CA
-- 2 AB, 3 BC, 4 CA => 1 FUEL
--
-- 1 FUEL => 2 AB + 3 BC + 4 CA
-- 1 FUEL => 2 * (3 A + 4 B) + 3 * (5 B + 7 C) + 4 * (4 C + 1 A)
-- 1 FUEL => (6 A + 8 B) + (15 B + 21 C) + (16 C + 4 A)
-- 1 FUEL => 6 A + 8 B + 15 B + 21 C + 16 C + 4 A
-- 1 FUEL => 10 A + 23 B + 37 C
-- 1 FUEL => (CEIL 10/2 * 9 ORE) + (CEIL 23/3 * 8 ORE) + (CEIL 37/5 * 7 ORE)
-- 1 FUEL => 45 ORE + 64 ORE + 56 ORE
-- 1 FUEL => 165 ORE

findRecipe :: String -> [[(Int, String)]] -> Maybe [(Int, String)]
findRecipe _ [] = Nothing
findRecipe target (all@((_, name) : _) : xs) =
  if name == target then Just $ tail all else findRecipe target xs

parseInstruction :: String -> (Int, String)
parseInstruction x = (read num, name) where (num : name : []) = words x

splitLHS :: [String] -> [String]
splitLHS (x : xs) = header ++ xs where header = (splitOn "," x)

parseLines :: String -> [[(Int, String)]]
parseLines l =
  map (map parseInstruction)
    $ map (map strip . reverse)
    $ map splitLHS
    $ map (splitOn "=>")
    . lines
    $ l

getIngredients :: [[(Int, String)]] -> (Int, String) -> [(Int, String)]
getIngredients recipes (quantity, name) = case (findRecipe name recipes) of
  Just match -> match
  Nothing    -> [(quantity, name)]

replaceSubIngredients recipes equation = 
    concat [ if b == "ORE" then [x] else new | x <- equation, let new@((a,b):_) = getIngredients recipes x]

reduceRecipe :: [[(Int, String)]] -> [(Int, String)] -> [(Int, String)]
reduceRecipe recipes equation = 
  replaceSubIngredients recipes equation

part1 :: IO ()
part1 = do
  input <- readFile "inputs/day14.txt"
  let parsed = parseLines input in print $ findRecipe "FUEL" parsed
