{-# LANGUAGE InstanceSigs #-}

module ParserCombinators where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (replicateM, (>=>))
import           Data.Char           (isDigit, isSpace)

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

--Task 1
instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser rp) = Parser $ \xs -> fmap (first f) (rp xs)
      where
        first :: (a -> b) -> (a, c) -> (b, c)
        first g (a, c) = (g a, c)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \xs -> Just (a, xs)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    (<*>) (Parser rf) (Parser ra) = Parser $ rf >=>
                                    \(f, t) -> ra t >>=
                                    \(a, r) -> return (f a, r)

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    (>>=) (Parser rf) g = Parser $ \xs ->
          case rf xs of
            Just (f, a) -> runParser (g f) a
            Nothing     -> Nothing

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser (const Nothing)

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    (<|>) (Parser r) (Parser rr) = Parser $ \xs -> r xs <|> rr xs

--Task 2
ok :: Parser s ()
ok = Parser $ \xs -> Just ((), xs)

eof :: Parser s ()
eof = Parser $ \xs -> case xs of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \xs -> case xs of
    []     -> Nothing
    (y:ys) -> if p y
              then Just (y, ys)
              else Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s  => [s] -> Parser s [s]
stream = traverse element

--Task 3
parseBrackets :: Parser Char ()
parseBrackets = doParsing *> eof
  where
    doParsing :: Parser Char ()
    doParsing = (element '(' *> doParsing <* element ')')
                *> doParsing -- for this: ()()
                <|> ok

parseInt :: Parser Char Int
parseInt = read <$> ((:) <$> parseSign <*> some (satisfy isDigit))
  where
    parseSign = element '-' <|> emptySign
    emptySign = const ' ' <$> element '+' <|> const ' ' <$> ok -- not good, but read doesn't parse "+5"

--Task 4
parseLists :: Parser Char [[Int]]
parseLists = (:) <$> (parseSpaces *> parseList) <*> many (parseComma *> parseList)
  where
    parseList    = parseInt >>= (\i -> replicateM i (parseComma *> parseInt))
    parseComma   = parseSpaces *> element ',' <* parseSpaces
    parseSpaces  = many (satisfy isSpace)
