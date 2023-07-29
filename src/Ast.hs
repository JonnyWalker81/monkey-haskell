module Ast where

import Token

data Identifier = Identifier String deriving (Show, Eq)

data Block = Block [Statement] deriving (Show, Eq)

data Statement
  = Let {name :: Identifier, value :: Expression}
  | Return Expression
  | ExpressionStatement Expression
  | BlockStatement Block
  deriving (Show, Eq)

data Expression
  = Ident Identifier
  | Integer Int
  | Boolean Bool
  | String String
  | Prefix {operator :: Token, right :: Expression}
  | InfixExpr {left :: Expression, operator :: Token, right :: Expression}
  | If {condition :: Expression, consequence :: Block, alternative :: Maybe Block}
  | FunctionLiteral {parameters :: [Identifier], body :: Block}
  | Call {func :: Expression, arguments :: [Expression]}
  | Array [Expression]
  | Index {left :: Expression, index :: Expression}
  | Hash (Expression, Expression)
  deriving (Show, Eq)

-- data Infix
--   = Plus
--   | Minus
--   | Divide
--   | Multiply
--   | Eq
--   | NotEq
--   | GreaterThan
--   | LessThan
--   deriving (Show, Eq)

data Program = Program {statements :: [Statement]} deriving (Show)

data Node
  = Prog Program
  | Stmt Statement
  | Expr Expression
