{-# LANGUAGE FlexibleInstances #-}

module Lexer where

-- import Data.Text
import Control.Applicative
import Data.Char
import Debug.Trace
import Token

data LexError = Unexpected String

instance Show LexError where
  show (Unexpected msg) = "Unexpected: " ++ msg

newtype Lexer a = Lexer {runLexer :: String -> Either LexError (String, a)}

instance Functor Lexer where
  fmap f (Lexer p) = Lexer $ \input -> do
    (input', x) <- p input
    Right (input', f x)

instance Applicative Lexer where
  pure x = Lexer $ \input -> Right (input, x)
  (Lexer p1) <*> (Lexer p2) = Lexer $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Right (input'', f a)

instance Alternative (Either LexError) where
  empty = Left $ Unexpected "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Lexer where
  empty = Lexer $ const empty
  (Lexer p1) <|> (Lexer p2) = Lexer $ \input -> p1 input <|> p2 input

instance Monad Lexer where
  return = pure
  (Lexer p) >>= f = Lexer $ \input -> do
    (rest, x) <- p input
    runLexer (f x) rest

char :: Char -> Lexer Char
char c =
  Lexer $
    \input -> case input of
      (c' : rest) | c == c' -> Right (rest, c')
      _ -> Left $ Unexpected ("expected char")

string :: String -> Lexer String
string = traverse char

satisfy :: (Char -> Bool) -> Lexer Char
satisfy pred = Lexer $ \input ->
  case input of
    (c : rest) | pred c -> Right (rest, c)
    _ -> Left $ Unexpected ("did not satisfy")

tokenIf :: (Token -> Bool) -> Lexer Token
tokenIf predicate = Lexer $ \input ->
  case runLexer peek input of
    Right (rest, token) | predicate token -> Right (rest, token)
    _ -> Left $ Unexpected ("error tokenIf")

while :: (Char -> Bool) -> Lexer String
while f = Lexer $ \input ->
  let (rest, x) = span f input
   in Right (rest, x)

spaces :: Lexer String
spaces = many (satisfy isSpace)

plus :: Lexer Token
plus = Plus <$ char '+'

eof :: Lexer Token
eof = Eof <$ string ""

asterisk :: Lexer Token
asterisk = Asterisk <$ char '*'

assign :: Lexer Token
assign = Assign <$ char '='

leftParen :: Lexer Token
leftParen = LeftParen <$ char '('

rightParen :: Lexer Token
rightParen = RightParen <$ char ')'

leftBrace :: Lexer Token
leftBrace = LeftBrace <$ char '{'

rightBrace :: Lexer Token
rightBrace = RightBrace <$ char '}'

comma :: Lexer Token
comma = Comma <$ char ','

semicolon :: Lexer Token
semicolon = Semicolon <$ char ';'

bang :: Lexer Token
bang = (NotEqual <$ string "!=") <|> Bang <$ char '!'

equal :: Lexer Token
equal = (Equal <$ string "==") <|> assign

identifier :: Lexer Token
identifier = identToken <$> some (satisfy isIdent)

number :: Lexer Token
number = Number <$> some (satisfy isNumber)

stringLiteral :: Lexer Token
stringLiteral = StringLiteral <$> (char '"' *> readString <* char '"')

readString :: Lexer String
readString = many (satisfy isValidStringChar)

isValidStringChar :: Char -> Bool
isValidStringChar c = case c of
  '\0' -> Prelude.False
  '"' -> Prelude.False
  _ -> Prelude.True

isIdent :: (Char -> Bool)
isIdent c = case c of
  c | isLetter c -> Prelude.True
  '_' -> Prelude.True
  _ -> Prelude.False

readIdent :: Lexer String
readIdent = many (satisfy isIdent)

peek :: Lexer Token
peek = Lexer $ \input -> case runLexer next input of
  Right (_, token) -> Right (input, token)
  Left _ -> Left $ Unexpected ("error peeking")

next :: Lexer Token
next =
  spaces
    *> ( asterisk
           <|> plus
           <|> equal
           <|> leftParen
           <|> rightParen
           <|> leftBrace
           <|> rightBrace
           <|> comma
           <|> semicolon
           <|> LeftBracket <$ char '['
           <|> RightBracket <$ char ']'
           <|> Colon <$ char ':'
           <|> Minus <$ char '-'
           <|> bang
           <|> Slash <$ char '/'
           <|> LessThan <$ char '<'
           <|> GreaterThan <$ char '>'
           <|> identifier
           <|> number
           <|> stringLiteral
       )
    <* spaces

tokenize :: String -> [Token]
tokenize input = case runLexer tokens input of
  Right (_, tokens') -> (tokens' ++ [Eof])
  Left err -> error ("lexer error")
  where
    tokens = many next
