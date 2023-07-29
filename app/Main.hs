module Main where

import Lexer (tokenize)
import System.Environment
import System.IO
import Text.Printf
import Token (Token)

prompt :: String
prompt = ">> "

promptUser :: IO String
promptUser = do
  printf "%s" prompt
  hFlush stdout
  getLine

toStringList :: [Token] -> [String]
toStringList tokens =
  map show tokens

repl :: IO ()
repl = do
  input <- promptUser
  tokens <- pure $ tokenize input
  mapM_ (printf "%s\n") (map show tokens)
  repl

main :: IO ()
main = do
  user <- getEnv "USER"
  printf "Hello %s! This is the Monkey Programming Language!\n" user
  printf "Feel free to type in commands\n"
  repl
