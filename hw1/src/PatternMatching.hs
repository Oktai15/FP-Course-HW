module PatternMatching
       ( removeByIndex
       , mergeSort
       ) where

import           Data.List     (length)
import           Data.Maybe    (listToMaybe)

removeByIndex :: Int -> [a] -> ([a], Maybe a)
removeByIndex ind xs = (take ind xs ++ snd rightPart, listToMaybe $ fst rightPart)
  where
    rightPart = let x = drop ind xs in (take 1 x, drop 1 x)

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [v] = [v]
mergeSort l   = let (a, b) = splitAt (length l `div` 2) l
                in merge (mergeSort a) (mergeSort b)
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge xs [] = xs
    merge [] ys = ys
    merge f@(x:xs) s@(y:ys)
          | x <= y    = x:merge xs s
          | otherwise = y:merge f ys
