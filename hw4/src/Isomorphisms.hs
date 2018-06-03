{-# LANGUAGE RankNTypes #-}

module Isomorphisms where

import           Data.Functor.Const    (Const (..), getConst)
import           Data.Functor.Identity (Identity (..), runIdentity)
import           Data.Profunctor       (Profunctor (..))
import           Data.Tagged           (Tagged (..), untag)
import           Data.Tree             (Tree (..))
import           FSLens

type Iso b a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)

-- Task 6: helper functions
getTo :: Iso b a -> b -> a
getTo i = getConst . i Const

getFrom :: Iso b a -> a -> b
getFrom i = runIdentity . untag . i . Tagged . Identity

treeToFS :: Tree FilePath -> FS
treeToFS tr = if sf == [] then File (rootLabel tr) else Dir (rootLabel tr) (map treeToFS sf)
              where
                sf = subForest tr

fSToTree :: FS -> Tree FilePath
fSToTree (File p)      = Node { rootLabel = p, subForest = [] }
fSToTree (Dir p items) = Node { rootLabel = p, subForest = map fSToTree items }

-- Task 6: general functions
iso :: (b -> a) -> (a -> b) -> Iso b a
iso s e = dimap s (fmap e)

from :: Iso b a -> Iso a b
from i = iso (getFrom i) (getTo i)

isoTreeAndFS :: Iso (Tree FilePath) FS
isoTreeAndFS = iso treeToFS fSToTree
