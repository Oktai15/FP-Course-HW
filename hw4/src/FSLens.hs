{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module FSLens where

import           Control.Lens
import           Control.Monad    (forM)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import qualified System.FilePath  as FPath

data FS
    = Dir
          { _name     :: FilePath
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath
          }
    deriving (Eq, Show)

-- Task 2
getDirectory :: FilePath -> IO FS
getDirectory d = Dir (FPath.takeDirectory d) <$> getNodes d
  where
    getNodes dir = do
                   content <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
                   forM content (\c -> do
                                       let one_c = dir FPath.</> c
                                       isDirc <- doesDirectoryExist one_c
                                       if isDirc
                                       then fmap (Dir c) . getNodes $ one_c
                                       else return $ File c)

makeLenses ''FS
makePrisms ''FS

-- Task 3: helper functions
isFile :: FS -> Bool
isFile (File _)  = True
isFile (Dir _ _) = False

isDir :: FS -> Bool
isDir = not . isFile

-- Task 3: general functions
cd :: FilePath -> Traversal' FS FS
cd path = contents
          . traversed
          . filtered (\a -> isDir a && a ^. name == path)

ls :: Traversal' FS FS
ls = contents . each

file :: FilePath -> Traversal' FS FS
file path = contents
            . traversed
            . filtered (\a -> isFile a && a ^. name == path)

-- Task 4: Basic
changeExtensions :: String -> FS -> FS
changeExtensions ext = contents
                       . traversed
                       . filtered isFile
                       . name %~ flip FPath.replaceExtension ext

recLs :: FS -> [FilePath]
recLs fs = getFilenames ++ (getDirs >>= recLs)
  where
    getFilenames = fs ^.. contents . traversed . filtered isFile . name
    getDirs      = fs ^.. contents . traversed . filtered isDir

rmEmptyDirectory :: FilePath -> FS -> FS
rmEmptyDirectory p fs = fs & contents .~ nc
  where
    nc = fs ^.. contents . traversed . filtered (\a -> not $ a ^. name == p && null (a ^. contents))

