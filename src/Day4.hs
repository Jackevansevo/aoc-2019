module Day4 where

import           Data.List

equalPair :: Eq a => (a, a) -> Bool
equalPair x = fst x == snd x

pairs :: [a] -> [(a, a)]
pairs a = zip a $ tail a

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

day4part1 = print $ length [ x | x <- [130254 .. 678275], valid $ digits x ]

adjEqualPairs a = map fst [ (x, y) | (x, y) <- pairs $ a, x == y ]

frequency list = map length $ (group list)

countAdjacentPairs x = frequency $ adjEqualPairs x

valid input | a > b || b > c || c > d || d > e || e > f = False
            | otherwise = (a == b || b == c || c == d || d == e || e == f)
  where (a : b : c : d : e : f : []) = input


-- TODO: 677788 Passes, when it shouldn't
part1 = print $ length [ x | x <- [130254 .. 678275], valid $ digits x ]

-- part2 = print $ length
--   [ x | x <- [130254 .. 678275], let d = digits x, newValid d, valid d ]
