{-# LANGUAGE InstanceSigs #-}

module Monoids where

import           Data.Either    (lefts, rights)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup (..))

data NonEmpty   a   = a :| [a]
data ThisOrThat a b = This a | That b | Both a b
data Builder        = One Char | Many [Builder] deriving Show

newtype Name        = Name String
newtype Endo a      = Endo { getEndo :: a -> a }

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
    (This a1) <> (This a2)       = This (a1 <> a2)
    (That b1) <> (That b2)       = That (b1 <> b2)
    (This a) <> (That b)         = Both a b
    (That b) <> (This a)         = Both a b
    (Both a1 b1) <> (Both a2 b2) = Both (a1 <> a2) (b1 <> b2)
    (Both a1 b) <> (This a2)     = Both (a1 <> a2) b
    (Both a b1) <> (That b2)     = Both a (b1 <> b2)
    (This a2) <> (Both a1 b)     = Both (a1 <> a2) b
    (That b2) <> (Both a b1)     = Both a (b1 <> b2)

instance Semigroup Builder where
    (One a)   <> (One b)  = Many [One a, One b]
    o@(One _) <> (Many l) = Many (o : l)
    (Many l)  <> o@(One _)  = Many (l ++ [o])
    (Many l)  <> (Many x) = Many (l ++ x)

instance Monoid Builder where
    mempty              = Many []
    mappend a (Many []) = a
    mappend (Many []) a = a
    mappend a b         = a <> b

instance Semigroup Name where
    (Name a) <> (Name b) = Name $ a ++ ('.' : b)

instance Monoid Name where
    mempty              = Name ""
    mappend a (Name "") = a
    mappend (Name "") b = b
    mappend a b         =  a <> b

instance Semigroup (Endo a) where
    (Endo x) <> (Endo y) = Endo (x . y)

instance Monoid (Endo a) where
    mempty = Endo id
    mappend = (<>)

-- Task 1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat l = fromMaybe [] (mconcat l)

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat l = (mconcat $ lefts l, mconcat $ rights l)

-- Task 3
fromString :: String -> Builder
fromString []     = Many []
fromString [c]    = One c
fromString (c:cs) = Many (One c : [fromString cs])

toString :: Builder -> String
toString (One c)   = [c]
toString (Many []) = ""
toString (Many l)  = foldr (mappend . toString) mempty l