main = do
    w <- getLine
    let d = (map read) . words $ w
    let x = d !! 0
    let y = d !! 1
    let a = (log x) * y
    let b = (log y) * x
    if (a < b)
    then putStr "<"
    else if (a == b)
         then putStr "="
         else putStr ">"
