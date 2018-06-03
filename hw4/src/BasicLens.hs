{-# LANGUAGE RankNTypes #-}

module BasicLens where

import           Control.Applicative    (Const (..))
import           Control.Arrow          ((&&&))
import           Control.Monad.Identity (Identity (..))

-- Real Lens
type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

-- Simple Lens
type Lens' s a = Lens s s a a

-- Task 1: Basic (commented code was written before hard tasks)

-- set :: Lens' s a -> a -> s -> s
-- set l b = runIdentity . l (\_ -> Identity b)

set :: Lens s t a b -> b -> s -> t
set l f = over l (const f)

-- view :: Lens' s a -> s -> a
-- view l = asks (getConst . l Const)

view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- over :: Lens' s a -> (a -> a) -> s -> s
-- over l f = runIdentity . l (Identity . f)

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- (.~) :: Lens' s a -> a -> s -> s
(.~) :: Lens s t a b -> b -> s -> t
(.~) = set

-- (^.) :: s -> Lens' s a -> a
(^.) :: s -> Lens s t a b -> a
(^.) s l = view l s

-- (%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 f s = (\t b -> (b, snd t)) s <$> f (fst s)

_2 :: Lens (x, a) (x, b) a b
_2 f s = (\t b -> (fst t, b)) s <$> f (snd s)

-- Task 1: Hard
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get_ set_ = \a b -> fmap (set_ b) (a (get_ b))

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens get_ set_
  where
    get_ = either (view l1) (view l2)
    set_ = either (\a b -> Left (set l1 b a)) (\a b -> Right (set l2 b a))

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (id &&& f)
