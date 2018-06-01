import Control.Monad
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

main = do
    n <- fmap read getLine
    w <- getLine
    let pow2 = [2 ^ k | k <- [0, 1 .. 32]]
    let ints = (map read) . words $ w
    let sort_ints = sort ints
    let s = Set.fromList sort_ints
    let ans = map (findPoints s pow2) sort_ints
    --- :(

findPoints :: Set Int -> [Int] -> Int -> [Int]
findPoints s p2 start = do
                        let np = [start + i | i <- p2]
                        --- :(
