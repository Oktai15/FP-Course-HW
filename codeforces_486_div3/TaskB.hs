import Control.Monad
import Data.List
import Data.Ord (comparing)

main = do
    n <- fmap read getLine
    strings <- replicateM n getLine
    let sort_strings = lsort strings
    let len = length sort_strings
    if (check sort_strings len || (len == 1))
    then do
         putStrLn "YES"
         mapM_ putStrLn sort_strings
    else do
         putStr "NO"
         --mapM_ putStrLn sort_strings

lsort :: [String] -> [String]
lsort = sortBy (comparing length)

check :: [String] -> Int -> Bool
check _ 1 = True
check (x:y:xs) l = if isInfixOf x y
                   then check (y:xs) (l-1)
                   else False