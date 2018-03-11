{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foldable where

data Pair a = Pair a a deriving Show
data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f b (Pair x1 x2) = f x1 (f x2 b)

  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x1 x2) = mappend (f x1) (f x2)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f b (x :| xs) = f x (foldr f b xs)

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = mappend (f x) (foldMap f xs)

--Task2
splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn delim str = let (f, s) = foldr splitHelper ([],[]) str in s :| f
  where
    splitHelper :: a -> ([[a]], [a]) -> ([[a]], [a])
    splitHelper newElem accum
        | newElem == delim = (snd accum : fst accum, [])
        | otherwise        = (fst accum, newElem : snd accum)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith delim = foldr1 joinWithHelper
  where
    joinWithHelper accum newElem = accum ++ [delim] ++ newElem
