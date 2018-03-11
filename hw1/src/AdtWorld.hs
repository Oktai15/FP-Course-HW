{-# LANGUAGE InstanceSigs #-}

module AdtWorld where

import           Data.Either        (fromRight, isLeft, isRight)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as Ne
import           Data.Maybe         (fromJust)

data DayOfWeek = Mn | Tu | Wd | Th | Fr | St | Sn
    deriving (Eq, Enum, Show)

data Village = Village
               { mbCastle          :: Maybe Castle
               , mbLibraryOrChurch :: Maybe LibraryOrChurch
               , houses            :: Houses
               } deriving (Show, Eq)

data Castle = Castle
              { isLord  :: Bool
              , isWalls :: Bool
              } deriving (Show, Eq)

data Houses = OneHouse Family | ManyHouses Family Houses deriving (Show, Eq)
data Family = One | Two | Three | Four deriving (Enum, Show, Eq)
data LibraryOrChurch = Library | Church deriving (Show, Eq)
type Cause = String

data Nat = Z | S Nat deriving (Show)

instance Num Nat where
    (+) :: Nat -> Nat -> Nat
    (+) x Z     = x
    (+) Z y     = y
    (+) x (S b) = S (x + b)

    (-) :: Nat -> Nat -> Nat
    (-) Z _         = Z
    (-) x Z         = x
    (-) (S a) (S b) = a - b

    (*) :: Nat -> Nat -> Nat
    (*) _ Z     = Z
    (*) Z _     = Z
    (*) x (S b) = x + (b * x)

    abs :: Nat -> Nat
    abs x = x

    signum :: Nat -> Nat
    signum Z = Z
    signum _ = S Z

    fromInteger :: Integer -> Nat
    fromInteger 0 = Z
    fromInteger x = S (fromInteger $ x - 1)

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    (==) Z Z         = True
    (==) Z (S _)     = False
    (==) (S _) Z     = False
    (==) (S x) (S y) = x == y

instance Ord Nat where
    (<=) :: Nat -> Nat -> Bool
    (<=) Z _         = True
    (<=) _ Z         = False
    (<=) (S x) (S y) = x <= y

data Tree a = Leaf | Node (NonEmpty a) (Tree a) (Tree a) deriving (Show, Eq)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf                    = mempty
    foldMap f (Node l tr1 tr2) = foldMap f tr1 `mappend` foldMap f l `mappend` foldMap f tr2

-- Task 1
nextDay :: DayOfWeek -> DayOfWeek
nextDay d
    | d == Sn   = Mn
    | otherwise = toEnum $ fromEnum d + 1

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays d s = toEnum $ fromEnum d + s `mod` 7

isWeekend :: DayOfWeek -> Bool
isWeekend d = d == St || d == Sn

daysToParty :: DayOfWeek -> Int
daysToParty d
    | fromEnum d >= 5 = (5 - t) + 1
    | otherwise       = t - 1
      where
        t = abs (5 - n)
        n = fromEnum d

-- Task 2
buildFirstCastle :: Village -> Either Village Village
buildFirstCastle v@Village{ mbCastle = Nothing } =
    Right v { mbCastle = Just Castle {isLord = False, isWalls = False}}
buildFirstCastle v@Village{ mbCastle = Just _ }  = Left v

buildFirstLibraryOrChurch :: LibraryOrChurch -> Village -> Either Village Village
buildFirstLibraryOrChurch lorc v@Village{ mbLibraryOrChurch = Nothing } =
    Right v { mbLibraryOrChurch = Just lorc }
buildFirstLibraryOrChurch _    v@Village{ mbLibraryOrChurch = Just _ }  = Left v

buildHouse :: Family -> Village -> Village
buildHouse f v@Village{ houses = hs } = v { houses = ManyHouses f hs}

joinLord :: Village -> Either (Village, Cause) Village
joinLord v@Village{ mbCastle = Nothing } = Left (v, "Village doesn't have a castle")
joinLord v@Village{ mbCastle = Just Castle{ isLord = True } } =
    Left (v, "Village has already had lord")
joinLord v@Village{ mbCastle = Just Castle{ isLord = False, isWalls = iw } } =
    Right v { mbCastle = Just(Castle True iw) }

buildWalls :: Village -> Either (Village, Cause) Village
buildWalls v@Village{ mbCastle = Nothing } = Left (v, "Village doesn't have a castle")
buildWalls v@Village{ mbCastle = Just Castle{ isLord = False } } =
    Left (v, "Village doesn't have lord")
buildWalls v@Village{ mbCastle = Just Castle{ isWalls = True } } =
    Left (v, "Village has already had walls")
buildWalls v@Village{ mbCastle = Just Castle{ isLord = True, isWalls = False }, houses = hs }
    | countPopulation hs >= 10 = Right v { mbCastle = Just(Castle True True) }
    | otherwise                = Left (v, "Village doesn't have enough people")
      where
        countPopulation :: Houses -> Int
        countPopulation (OneHouse f)      = countFamily f
        countPopulation (ManyHouses f fs) = countFamily f + countPopulation fs
        countFamily :: Family -> Int
        countFamily f = fromEnum f + 1

-- Task 3
toInteger :: Nat -> Integer
toInteger Z     = 0
toInteger (S x) = 1 + AdtWorld.toInteger x

isEven :: Nat -> Bool
isEven Z     = True
isEven (S x) = not $ isEven x

natDiv :: Nat -> Nat -> Either String Nat
natDiv _ Z         = Left "Division by zero"
natDiv Z _         = Right Z
natDiv x y
    | sbt > Z   = if isRight nd
                  then Right $ S Z + fromRight (S Z) nd
                  else Left "Something strange happened..."
    | x == y    = Right $ S Z
    | otherwise = Right Z
      where
        sbt = x - y
        nd  = natDiv sbt y

natMod :: Nat -> Nat -> Either String Nat
natMod x y
    | isLeft nd = nd
    | otherwise = Right $ x - (rnd * y)
      where
        nd  = natDiv x y
        rnd = fromRight (S Z) (natDiv x y)

-- Task 4
isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _    = False

sizeTree :: Tree a -> Int
sizeTree Leaf              = 0
sizeTree (Node xs tr1 tr2) = length xs + sizeTree tr1 + sizeTree tr2

findElem :: Ord a => a -> Tree a -> Maybe (Tree a)
findElem _ Leaf = Nothing
findElem v n@(Node xs tr1 tr2)
    | currentElem == v = Just n
    | currentElem < v  = findElem v tr2
    | otherwise        = findElem v tr1
      where
        currentElem = Ne.head xs

minimumTree :: Tree a -> Maybe (Ne.NonEmpty a)
minimumTree Leaf             = Nothing
minimumTree (Node xs Leaf _) = Just xs
minimumTree (Node _ tr1 _)   = minimumTree tr1

insertElem :: Ord a => a -> Tree a -> Tree a
insertElem v Leaf = Node (v :| []) Leaf Leaf
insertElem v (Node xs tr1 tr2)
    | currentElem == v = Node (v :| Ne.toList xs) tr1 tr2
    | currentElem < v  = Node xs tr1 (insertElem v tr2)
    | otherwise        = Node xs (insertElem v tr1) tr2
      where
        currentElem = Ne.head xs

deleteElem :: Ord a => a -> Tree a -> Tree a
deleteElem _ Leaf = Leaf
deleteElem v (Node xs tr1 tr2)
    | currentElem == v = deleteHelper xs tr1 tr2
    | currentElem < v  = Node xs tr1 (deleteElem v tr2)
    | otherwise        = Node xs (deleteElem v tr1) tr2
      where
        deleteHelper :: Ord a => Ne.NonEmpty a -> Tree a -> Tree a -> Tree a
        deleteHelper (_:|(y:ys)) l r = Node (y:|ys) l r
        deleteHelper _          l r
            | not (isTr1 || isTr2) = let m = fromJust $ minimumTree r
                                     in Node m l (deleteElem (Ne.head m) r)
            | isTr1                = r
            | otherwise            = l
        currentElem = Ne.head xs
        isTr1 = isEmptyTree tr1
        isTr2 = isEmptyTree tr2

fromList :: Ord a => [a] -> Tree a
fromList = foldr insertElem Leaf
