import Control.Monad
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

main = do
    let h = Set.empty
    w1 <- getLine
    let d = (map read) . words $ w1
    let n = d !! 0
    let k = d !! 1
    w2 <- getLine
    let a = (map read) . words $ w2
    let ans = fromMaybe [] (getList a k 1 [] h)
    if ans == []
    then putStr "NO"
    else do
         putStrLn "YES"
         putStrLn $ list_to_string $ ans

getList :: [Int] -> Int -> Int -> [Int] -> Set Int -> Maybe [Int]
getList _ 0 _ ans _ = Just ans
getList [] k _ _  _ = Nothing
getList (x:xs) k count ans s = if Set.member x s
                             then getList xs k (count+1) ans s
                             else do
                                  let new_s = Set.insert x s
                                  getList xs (k-1) (count+1) (count : ans) new_s

list_to_string = unwords . map show