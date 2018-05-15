{-# LANGUAGE FlexibleContexts #-}

module ComputationWithState where

import           Control.Applicative        (empty)
import           Control.Monad              (forM_, void)
import           Control.Monad.Except       (MonadError, liftEither, runExceptT,
                                             throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks, liftM2, local,
                                             runReader)
import           Control.Monad.State        (MonadState, get, gets, modify,
                                             runStateT)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isNothing)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, many,
                                             notFollowedBy, parseMaybe,
                                             sepEndBy, skipSome, try, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             letterChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)

data Expr = Lit Int
            | Var String
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Let String Expr Expr deriving (Show)

data Frame = Wr Expr | Rd String | Upd String Expr | Mut String Expr deriving (Show)

keywords :: [String]
keywords = ["let", "in", "mut"]

type Environment = Map.Map String Int

data ProgramError = VariableNotFound String
                    | DivisionByZero
                    | VariableAlreadyInScope String
                    | VariableNotInScope String
                    | ParseError deriving (Eq, Show)

type Parser = Parsec Void String

--Task 1
eval :: (MonadReader Environment m, MonadError ProgramError m) => Expr -> m Int
eval (Lit v)  = return v
eval (Var s)  = do varValue <- asks (Map.lookup s)
                   maybe (throwError $ VariableNotFound s) return varValue
eval (Add a b) = liftM2 (+) (eval a) (eval b)
eval (Sub a b) = liftM2 (-) (eval a) (eval b)
eval (Mul a b) = liftM2 (*) (eval a) (eval b)
eval (Div a b) = eval a >>= (\a'-> eval b >>= \b' -> if b' == 0
                                                     then throwError DivisionByZero
                                                     else return $ a' `div` b')
eval (Let x d e) = do m <- eval d
                      local (Map.insert x m) (eval e)

{- Example:
   expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))
   env = (Map.fromList [("x", 1)])
   runEval expr env
-}
runEval :: Expr -> Environment -> Either ProgramError Int
runEval expr env = runReader (runExceptT (eval expr)) env

--Task 2

--Task 2: helper functions and fields
aOperations :: [[Operator Parser Expr]]
aOperations =
  [ [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-")]
  ]

myPSpaces :: Parser ()
myPSpaces = skipSome (char ' ')

pSpace :: Parser ()
pSpace = L.space myPSpaces empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme pSpace

symbol :: String -> Parser String
symbol = L.symbol pSpace

integer :: Parser Int
integer = lexeme L.decimal

brackets :: Parser a -> Parser a
brackets = between (symbol "(") (symbol ")")

keyword :: String -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

--Task 2: main functions
{-
   Example:
   expr = "let z = x + y in z * z"
   parsedExpr = Data.Maybe.fromJust $ Text.Megaparsec.parseMaybe pExpr expr
   env = (Map.fromList [("x", 1), ("y", 2)])
   runEval parsedExpr env
-}

pExpr :: Parser Expr
pExpr = pSpace *> makeExprParser pTerm aOperations

pTerm :: Parser Expr
pTerm = try pLet <|> brackets pExpr <|> pLit <|> pVar

pLit :: Parser Expr
pLit = Lit <$> integer

pVar :: Parser Expr
pVar = Var <$> identifier

pLet :: Parser Expr
pLet = do keyword "let"
          x <- identifier
          void (symbol "=")
          d <- pExpr
          keyword "in"
          e <- pExpr
          return (Let x d e)

--Task 3
upd :: (MonadState Environment m, MonadError ProgramError m) => String -> Int -> m ()
upd k v = do c <- gets (Map.lookup k)
             if isNothing c
             then throwError $ VariableNotInScope k
             else modify (Map.insert k v)

mut :: (MonadState Environment m, MonadError ProgramError m) => String -> Int -> m ()
mut k v = do c <- gets (Map.lookup k)
             if isNothing c
             then modify (Map.insert k v)
             else throwError $ VariableAlreadyInScope k

--Task 4
pMut :: Parser Frame
pMut = Mut <$> (keyword "mut" *> identifier <* symbol "=") <*> pExpr

pUpd :: Parser Frame
pUpd = Upd <$> (identifier <* symbol "=") <*> pExpr

pWr :: Parser Frame
pWr = Wr <$> (symbol "<" *> pExpr)

pRd :: Parser Frame
pRd = Rd <$> (symbol ">" *> identifier)

pFrame :: Parser Frame
pFrame = pMut <|> pUpd <|> pWr <|> pRd

pFrames :: Parser [Frame]
pFrames = sepEndBy (pSpace *> pFrame) eol

--Task 5, 6, 7 and 10
rd :: (MonadState Environment m) => String -> Int -> m ()
rd k v = modify $ Map.insert k v

wr :: (MonadIO m) => Int -> m ()
wr v = liftIO $ print v

execFrame :: (MonadIO m, MonadState Environment m, MonadError ProgramError m) => Frame -> m Int
execFrame frame = do env <- get
                     case frame of
                       (Mut _ expr) -> liftEither $ runEval expr env
                       (Upd _ expr) -> liftEither $ runEval expr env
                       (Rd  _     ) -> read <$> liftIO getLine
                       (Wr    expr) -> liftEither $ runEval expr env

execFrames :: (MonadIO m, MonadState Environment m, MonadError ProgramError m) => [Frame] -> m ()
execFrames frames = forM_ frames $ \frame -> do v <- execFrame frame
                                                case frame of
                                                  Mut k _ -> mut k v
                                                  Upd k _ -> upd k v
                                                  Rd  k   -> rd  k v
                                                  Wr    _ -> wr  v

{-
Example:
    f1 = Mut "x" (Lit 5)
    f2 = Mut "y" (Lit 6)
    f3 = Wr (Var "x")
    f4 = Wr (Var "y")
    f5 = Wr (Var "y" `Add` Var "x")
    runFrames [f1, f2, f3, f4, f5] Map.empty
-}
runFrames :: MonadIO m => [Frame] -> Environment -> m (Either ProgramError ())
runFrames frames env = do runExceptT $ fst <$> runStateT (execFrames frames) env

parseProgram :: String -> Maybe [Frame]
parseProgram stringWithProgram = parseMaybe pFrames stringWithProgram

{-
Example:
    runProgram "mut x = 5 \n < x + 3 \n"
-}
runProgram :: MonadIO m => String -> m (Either ProgramError ())
runProgram stringWithProgram = let frames = parseProgram stringWithProgram in
                                 case frames of
                                   Nothing   -> do return $ Left $ ParseError
                                   (Just xs) -> runFrames xs Map.empty

{-
Example:
    runProgramFromFile "ProgramExample.txt"
-}
runProgramFromFile :: FilePath -> IO (Either ProgramError ())
runProgramFromFile path = do str <- readFile path
                             runProgram str
