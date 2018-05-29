import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

main = do
    let c = ["purple", "green", "blue", "orange", "red", "yellow"]
    let h = Map.fromList [("purple", "Power"),
                          ("green", "Time"),
                          ("blue", "Space"),
                          ("orange", "Soul"),
                          ("red", "Reality"),
                          ("yellow", "Mind")]
    n <- fmap read getLine
    a <- replicateM n getLine
    let xs = c \\ a
    print $ length xs
    mapM_ putStrLn (map (\k -> Map.findWithDefault "def" k h) xs)
