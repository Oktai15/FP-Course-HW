import qualified Data.Bimap as B

main = do
    n <- fmap read getLine
    w <- getLine
    let d = (map read) . words $ w
    if (go (B.fromList (zip [1..n] d) 0) `mod` 2 == 0)
    then putStr "Petr"
    else putStr "Um_nik"

--go f c = map

