{-# LANGUAGE QuasiQuotes #-}

module LexerSpec where

import qualified Control.Exception as Token
import Lexer
import Test.Syd
import Text.RawString.QQ
import qualified Token

spec :: Spec
spec =
  describe "Lexer" $
    do
      it "can parse char" $
        case runLexer (char 'c') "cba" of
          Right (rest, c) -> c == 'c'
          Left err -> False
      it
        "fails when no"
        $ case runLexer (char 'c') "abb" of
          Right _ -> undefined
          Left err -> True
      it "fails with empty string" $
        case runLexer (char 'c') "" of
          Left err -> True
          Right _ -> undefined
      it "parse string" $
        case runLexer (string "test") "test" of
          Right (rest, s) -> s == "test"
          Left _ -> False
      it "fails to parse string" $
        case runLexer (string "test") "foo" of
          Right _ -> undefined
          Left _ -> True

      it "parse asterisk" $
        case runLexer asterisk "*+" of
          Right (_, x) -> x `shouldBe` Token.Asterisk
          Left _ -> undefined

      it "parse plus" $
        case runLexer plus "+*" of
          Right (_, x) -> x `shouldBe` Token.Plus
          Left _ -> undefined

      it "next token" $
        case runLexer next "*+" of
          Right (rest, x) -> x `shouldBe` Token.Asterisk
          Left _ -> undefined

      it "next token again" $
        case runLexer next "*+" of
          Right (rest, x) -> case runLexer next rest of
            Right (_, y) -> y `shouldBe` Token.Plus
            Left _ -> undefined
          Left _ -> undefined

      it "parse tokens" $
        case tokenize [r|  =+(){},; == ![]:-/<>!= let if else fn return true false 1234 foo "bar-bar "|] of
          tokens ->
            tokens
              `shouldBe` [ Token.Assign,
                           Token.Plus,
                           Token.LeftParen,
                           Token.RightParen,
                           Token.LeftBrace,
                           Token.RightBrace,
                           Token.Comma,
                           Token.Semicolon,
                           Token.Equal,
                           Token.Bang,
                           Token.LeftBracket,
                           Token.RightBracket,
                           Token.Colon,
                           Token.Minus,
                           Token.Slash,
                           Token.LessThan,
                           Token.GreaterThan,
                           Token.NotEqual,
                           Token.Let,
                           Token.If,
                           Token.Else,
                           Token.Function,
                           Token.Return,
                           Token.True,
                           Token.False,
                           Token.Number "1234",
                           Token.Ident "foo",
                           Token.StringLiteral "bar-bar ",
                           Token.Eof
                         ]

      it "lex all the tokens" $ case tokenize
        [r|
                        let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
                        |] of
        tokens ->
          tokens
            `shouldBe` [ Token.Let,
                         Token.Ident "five",
                         Token.Assign,
                         Token.Number "5",
                         Token.Semicolon,
                         Token.Let,
                         Token.Ident "ten",
                         Token.Assign,
                         Token.Number "10",
                         Token.Semicolon,
                         Token.Let,
                         Token.Ident "add",
                         Token.Assign,
                         Token.Function,
                         Token.LeftParen,
                         Token.Ident "x",
                         Token.Comma,
                         Token.Ident "y",
                         Token.RightParen,
                         Token.LeftBrace,
                         Token.Ident "x",
                         Token.Plus,
                         Token.Ident "y",
                         Token.Semicolon,
                         Token.RightBrace,
                         Token.Semicolon,
                         Token.Let,
                         Token.Ident "result",
                         Token.Assign,
                         Token.Ident "add",
                         Token.LeftParen,
                         Token.Ident "five",
                         Token.Comma,
                         Token.Ident "ten",
                         Token.RightParen,
                         Token.Semicolon,
                         Token.Bang,
                         Token.Minus,
                         Token.Slash,
                         Token.Asterisk,
                         Token.Number "5",
                         Token.Semicolon,
                         Token.Number "5",
                         Token.LessThan,
                         Token.Number "10",
                         Token.GreaterThan,
                         Token.Number "5",
                         Token.Semicolon,
                         Token.If,
                         Token.LeftParen,
                         Token.Number "5",
                         Token.LessThan,
                         Token.Number "10",
                         Token.RightParen,
                         Token.LeftBrace,
                         Token.Return,
                         Token.True,
                         Token.Semicolon,
                         Token.RightBrace,
                         Token.Else,
                         Token.LeftBrace,
                         Token.Return,
                         Token.False,
                         Token.Semicolon,
                         Token.RightBrace,
                         Token.Number "10",
                         Token.Equal,
                         Token.Number "10",
                         Token.Semicolon,
                         Token.Number "10",
                         Token.NotEqual,
                         Token.Number "9",
                         Token.Semicolon,
                         Token.StringLiteral "foobar",
                         Token.StringLiteral "foo bar",
                         Token.LeftBracket,
                         Token.Number "1",
                         Token.Comma,
                         Token.Number "2",
                         Token.RightBracket,
                         Token.Semicolon,
                         Token.LeftBrace,
                         Token.StringLiteral "foo",
                         Token.Colon,
                         Token.StringLiteral "bar",
                         Token.RightBrace,
                         Token.Eof
                       ]
