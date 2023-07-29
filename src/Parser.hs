{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Ast (Expression)
import qualified Ast as Ast
import Control.Applicative
import Debug.Trace (trace)
import GHC.IO.Exception (IOErrorType (UnsatisfiedConstraints))
import Lexer (isIdent)
import Lexer hiding (Unexpected, next, peek, satisfy)
import Options.Applicative.Help (vsepChunks)
import qualified Token as T

data ParseError = Unexpected String

instance Show ParseError where
  show (Unexpected msg) = "Parse Error: Unexpected: " ++ msg

newtype Parser a = Parser {runParser :: T.Tokens -> Either ParseError (T.Tokens, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Right (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Right (input'', f a)

instance Alternative (Either ParseError) where
  empty = Left $ Unexpected "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input -> do
    (rest, x) <- p input
    runParser (f x) rest

data Precedence
  = Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
  | Index
  deriving (Show, Ord, Eq)

token_prec :: T.Token -> Precedence
token_prec t = case t of
  T.Equal -> Equals
  T.NotEqual -> Equals
  T.LessThan -> LessGreater
  T.GreaterThan -> LessGreater
  T.Plus -> Sum
  T.Minus -> Sum
  T.Slash -> Product
  T.Asterisk -> Product
  T.LeftParen -> Call
  T.LeftBracket -> Index
  _ -> Lowest

satisfy :: (T.Token -> Bool) -> Parser T.Token
satisfy pred = Parser $ \input ->
  case input of
    (t : rest) | pred t -> Right (rest, t)
    _ -> Left $ Unexpected ("parer: did not satisfy")

match :: T.Token -> Parser T.Token
match t = Parser $ \input -> case input of
  (t' : rest) | t' == t -> Right (rest, t')
  _ -> Left $ Unexpected ("expected a token: " ++ (show t))

chomp :: T.Token -> Parser T.Token
chomp t = Parser $ \input -> case input of
  (t' : rest) | t' == t -> Right (rest, t')
  _ -> Right (input, t)

peek :: Parser T.Token
peek = Parser $ \input -> case input of
  (c : p : rest) -> Right (input, p)
  _ -> Left $ Unexpected ("parse error: peek")

consume :: Parser T.Token
consume = Parser $ \input -> case input of
  (t : rest) -> Right (rest, t)
  _ -> Left $ Unexpected ("parse error: consume")

current :: Parser T.Token
current = Parser $ \input -> case input of
  (t : _) -> Right (input, t)
  _ -> Left $ Unexpected ("parse error: current")

identP :: Parser Ast.Identifier
identP =
  satisfy isIdent >>= \case
    (T.Ident t) -> return (Ast.Identifier t)
    _ -> error "Expected identifier"
  where
    isIdent (T.Ident _) = True
    isIdent _ = False

numberP :: Parser Expression
numberP =
  satisfy isNumber >>= \case
    (T.Number n) -> return (Ast.Integer $ read n)
    _ -> error "Expected number"
  where
    isNumber (T.Number _) = True
    isNumber _ = False

stringP :: Parser Expression
stringP =
  satisfy isString >>= \case
    (T.StringLiteral s) -> return (Ast.String s)
    _ -> error "parse error: error parsing string literal"
  where
    isString (T.StringLiteral _) = True
    isString _ = False

boolP :: Parser Expression
boolP =
  satisfy isBool >>= \case
    T.True -> return (Ast.Boolean True)
    T.False -> return (Ast.Boolean False)
    _ -> error "parse error: error parsing bool literal"
  where
    isBool (T.True) = True
    isBool (T.False) = True
    isBool _ = False

bangP :: Parser Expression
bangP = Ast.Prefix <$> match T.Bang <*> exprP Prefix

-- bangP = do
--   op <- match T.Bang
--   right <- exprP Prefix
--   return (Ast.Prefix T.Bang right)

minusP :: Parser Expression
minusP = Ast.Prefix <$> match T.Minus <*> exprP Prefix

-- minusP = do
--   op <- match T.Minus
--   right <- exprP Prefix
--   return (Ast.Prefix T.Minus right)

parseGroupedP :: Parser Expression
parseGroupedP = match T.LeftParen *> exprP Lowest <* match T.RightParen

identExprP :: Parser Expression
identExprP = Ast.Ident <$> identP

blockP :: Parser [Ast.Statement]
blockP = match T.LeftBrace *> many next <* match T.RightBrace

elseP :: Parser (Maybe Ast.Block)
elseP = (match T.Else *> (Just <$> Ast.Block <$> blockP)) <|> return (Nothing)

-- elseP =
--   ( do
--       match T.Else
--       Just <$> Ast.Block <$> blockP
--   )
--     <|> return Nothing

condP :: Parser Expression
condP = match T.If *> match T.LeftParen *> exprP Lowest <* match T.RightParen

ifP :: Parser Expression
ifP = Ast.If <$> condP <*> (Ast.Block <$> blockP) <*> elseP

-- ifP = do
--   cond <- match T.If *> match T.LeftParen *> exprP Lowest <* match T.RightParen
--   consequence <- blockP
--   Ast.If cond (Ast.Block consequence) <$> elseP

paramsP :: Parser [Ast.Identifier]
paramsP = match T.Function *> match T.LeftParen *> parseParameters <* match T.RightParen

functionP :: Parser Expression
functionP = Ast.FunctionLiteral <$> paramsP <*> (Ast.Block <$> blockP)

-- functionP = do
--   match T.Function
--   match T.LeftParen
--   params <- parseParameters
--   match T.RightParen
--   block <- blockP
-- return (Ast.FunctionLiteral params (Ast.Block block))

sepBy ::
  Parser a -> -- Parser for the separators
  Parser b -> -- Parser for elements
  Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseParameters :: Parser [Ast.Identifier]
parseParameters = sepBy (match T.Comma) identP

argsP :: Parser [Ast.Expression]
argsP = match T.LeftParen *> sepBy (match T.Comma) (exprP Lowest)

callP :: Expression -> Parser Expression
callP fn = Ast.Call fn <$> argsP

-- callP fn = do
--   match T.LeftParen
--   args <- sepBy (match T.Comma) (exprP Lowest)
--   trace ("args: " ++ (show args)) match T.RightParen
--   return (Ast.Call fn args)

prefixP :: Precedence -> Parser Expression
prefixP precedence =
  numberP
    <|> identExprP
    <|> stringP
    <|> bangP
    <|> minusP
    <|> boolP
    <|> parseGroupedP
    <|> ifP
    <|> functionP

infixP :: Expression -> Parser Expression
infixP left = do
  (precedence, maybeOp) <- infixOp <$> consume
  case maybeOp of
    Nothing -> error ("parse error: infix op")
    Just op -> do
      right <- exprP precedence
      return $ Ast.InfixExpr left op right

infixOp :: T.Token -> (Precedence, Maybe T.Token)
infixOp t = case t of
  T.Plus -> (Sum, Just T.Plus)
  T.Minus -> (Sum, Just T.Minus)
  T.Asterisk -> (Product, Just T.Asterisk)
  T.Slash -> (Product, Just T.Slash)
  T.GreaterThan -> (LessGreater, Just T.GreaterThan)
  T.LessThan -> (LessGreater, Just T.LessThan)
  T.Equal -> (Equals, Just T.Equal)
  T.NotEqual -> (Equals, Just T.NotEqual)
  T.LeftParen -> trace ("call infix") (Call, Nothing)
  _ -> (Lowest, Nothing)

exprP :: Precedence -> Parser Expression
exprP precedence = do
  left <- prefixP precedence
  loop precedence left
  where
    loop :: Precedence -> Expression -> Parser Expression
    loop precedence left = do
      maybeInfix <- infixOp <$> current
      case maybeInfix of
        (Call, _) | precedence < Call -> do
          left' <- trace ("callP...") callP left
          loop precedence left'
        (peekPrecedence, _) | precedence < peekPrecedence -> do
          left' <- infixP left
          loop precedence left'
        _ -> return left

letP :: Parser Ast.Statement
letP = do
  match T.Let
  name <- identP
  value <- match T.Assign *> exprP Lowest <* chomp T.Semicolon
  return (Ast.Let {Ast.name = name, Ast.value = value})

returnP :: Parser Ast.Statement
returnP = Ast.Return <$> (match T.Return *> exprP Lowest <* chomp T.Semicolon)

expressionStatementP :: Parser Ast.Statement
expressionStatementP = Ast.ExpressionStatement <$> exprP Lowest <* chomp T.Semicolon

next :: Parser Ast.Statement
next = letP <|> returnP <|> expressionStatementP

parse :: T.Tokens -> Ast.Program
parse tokens = case runParser parse' tokens of
  Right (_, s) -> Ast.Program s
  Left err -> error ("parse error")
  where
    parse' = many next
