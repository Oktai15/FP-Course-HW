{-# LANGUAGE InstanceSigs #-}

module HierarchyOfTypes where

import           Control.Applicative (liftA2)
import           Text.Read           (readMaybe)

newtype Optional a = Optional (Maybe (Maybe a))
data    NonEmpty a = a :| [a] deriving (Eq, Show)

--Task 1
stringSum :: String -> Maybe Int
stringSum s = let lOfMbInts = words s Prelude.>>= (\a -> [readMaybe a::Maybe Int])
              in foldr (liftA2 (+)) (Just 0) lOfMbInts

--Task 2
instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap _ (Optional Nothing)  = Optional Nothing
    fmap f (Optional (Just a)) = Optional $ Just $ fmap f a

instance Applicative Optional where
    pure :: a -> Optional a
    pure a = Optional $ Just $ Just a

    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    (<*>) (Optional Nothing)  _                   = Optional Nothing
    (<*>) (Optional (Just _)) (Optional Nothing)  = Optional (Just Nothing)
    (<*>) (Optional (Just f)) (Optional (Just a)) = Optional $ Just $ f <*> a

instance Monad Optional where
    return :: a -> Optional a
    return = pure -- ≡ Optional $ Just $ Just a                   -- (1)

    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    (>>=) (Optional (Just (Just a))) f = f a                      -- (2)
    (>>=) (Optional (Just Nothing))  _ = Optional (Just Nothing)  -- (3)
    (>>=) (Optional Nothing)         _ = Optional Nothing         -- (4)

-- Proving laws for Monad Optional
-- 1. Left Identity
-- LAW: return a >>= f ≡ f a
-- return a >>= f ≡
-- ≡ Optional (Just (Just a)) >>= f                               -- (1)
-- ≡ f a                                                          -- (2)
--
-- 2. Right Identity
-- LAW: m >>= return ≡ m
-- m ≡ Optional (Just (Just a))
--     Optional (Just (Just a))) >>= return ≡
--     ≡ return a                                                 -- (2)
--     ≡ Optional (Just (Just a))                                 -- (1)
-- m ≡ Optional (Just Nothing)
--     Optional (Just Nothing) >>= return ≡
--     ≡ Optional (Just Nothing)                                  -- (3)
-- m ≡ Optional Nothing
--     Optional Nothing >>= return ≡
--     ≡ Optional Nothing                                         -- (4)
--
-- 3. Associativity
-- LAW: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
-- m ≡ Optional (Just (Just a))
--     1. (Optional (Just (Just a)) >>= f) >>= g
--      ≡ f a >>= g                                               -- (2)
--     2. (Optional (Just (Just a)) >>= (\x -> f x >>= g)
--      ≡ (\x -> f x >>= g) a                                     -- (2)
--      ≡ f a >>= g                                               -- function application
--
-- m ≡ Optional (Just Nothing)
--     1. (Optional (Just Nothing) >>= f) >>= g
--      ≡ Optional (Just Nothing) >>= g                           -- (3)
--      ≡ Optional (Just Nothing)                                 -- (3)
--     2. Optional (Just Nothing) >>= (\x -> f x >>= g)
--      ≡ Optional (Just Nothing)                                 -- (3)
--
-- m ≡ Optional Nothing
--     1. ((Optional Nothing) >>= f) >>= g
--      ≡ (Optional Nothing) >>= g                                -- (4)
--      ≡ Optional Nothing                                        -- (4)
--     2. (Optional Nothing) >>= (\x -> f x >>= g)
--      ≡ Optional Nothing                                        -- (4)

instance Foldable Optional where
    foldMap :: Monoid m => (a -> m) -> Optional a -> m
    foldMap _ (Optional Nothing)         = mempty
    foldMap _ (Optional (Just Nothing))  = mempty
    foldMap f (Optional (Just (Just a))) = f a

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse f (Optional (Just (Just a))) = pure <$> f a
    traverse _ (Optional Nothing)         = pure $ Optional Nothing
    traverse _ (Optional (Just Nothing))  = pure $ Optional $ Just Nothing

--Task 3
instance Functor NonEmpty where
    fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
    fmap f (x :| xs) = f x :| fmap f xs

    (<$) :: a -> NonEmpty b -> NonEmpty a
    (<$) = fmap . const

instance Applicative NonEmpty where
    pure :: a -> NonEmpty a
    pure a = a :| []

    (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
    (<*>) (f :| fs) (g :| gs) = f g :| (([f] <*> gs) ++ (fs <*> g:gs))

instance Monad NonEmpty where
    return = pure

    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (>>=) (x :| [])     f = f x
    (>>=) (a :| (x:xs)) f = z :| (zs ++ [y] ++ ys)
      where
        (y :| ys) = (x :| xs) >>= f
        (z :| zs) = f a

instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f b (x :| xs) = f x (foldr f b xs)

    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x :| xs) = mappend (f x) (foldMap f xs)

instance Traversable NonEmpty where
    traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs