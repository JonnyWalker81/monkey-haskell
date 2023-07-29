module Token where

type Tokens = [Token]

data Token
  = Illegal
  | Ident String
  | Number String
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Colon
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  | If
  | Else
  | Return
  | True
  | False
  | Function
  | Let
  | StringLiteral String
  | Eof
  deriving (Show, Eq)

identToken input = case input of
  "if" -> If
  "else" -> Else
  "false" -> Token.False
  "true" -> Token.True
  "fn" -> Function
  "let" -> Let
  "return" -> Return
  _ -> Ident input
