module SimpleFunctions
       ( order3
       , smartReplicate
       , contains
       , stringSum
       ) where

import           Data.List (sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let [a1, b1, c1] = sort [a, b, c] in (a1, b1, c1)

smartReplicate :: Foldable t => t Int -> [Int]
smartReplicate = foldr (\x -> (++) (replicate x x)) []

contains :: (Foldable t, Eq a) => a -> [t a] -> [t a]
contains = filter . elem

stringSum :: String -> Int
stringSum s = sum $ map (\x -> read x::Int) (words s)
