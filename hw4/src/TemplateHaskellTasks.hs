{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellTasks where

import           Control.Monad       (replicateM)
import qualified Data.Text           as T
import qualified Language.Haskell.TH as TH

-- Task 1
chooseByIndices :: Int -> [Int] -> TH.Q TH.Exp
chooseByIndices n is = do
    xs <- replicateM n $ TH.newName "x"
    pure $ TH.LamE [TH.TupP $ map TH.VarP xs] (TH.TupE $ map (\i -> TH.VarE $ xs !! i) is)

-- Task 2
class (Show s) => MyShow s where
  myShow :: s -> T.Text

-- Example:
-- data MyData = Data1 | Data2 deriving (Show)
-- genMyShowInst ''MyData
-- myShow Data1
genMyShowInst :: TH.Name -> TH.Q [TH.Dec]
genMyShowInst t = TH.runQ [d|instance MyShow $(TH.conT t) where
                               myShow e = T.pack (show e) |]
