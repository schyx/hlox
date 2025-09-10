module Main (main) where

import Phases.Parser
import Phases.Scanner
import System.Environment
import System.Exit
import System.IO
import Tokens

main :: IO ()
main = do
  args <- getArgs
  checkArgs args

checkArgs :: [String] -> IO ()
checkArgs [] = runPrompt
checkArgs [file] = runFile file
checkArgs _ = do
  putStrLn "Usage: hlox [script]"
  exitWith $ ExitFailure 64

-- | Creates Lox repl
runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout -- required to get the `>` to show up
  input <- getLine
  run input
  runPrompt

-- | Runs a Lox file
runFile :: String -> IO ()
runFile filepath = do
  contents <- readFile filepath
  run contents

run :: String -> IO ()
run contents = do
  let tokens = scanTokens contents
  case tokens of
    Right tokes -> print $ parse tokes
    Left errs -> mapM_ putStrLn errs

toTestOutput :: Token -> String
toTestOutput token = show (tokenType token) ++ " " ++ lexeme token ++ " " ++ l
  where
    l
      | None <- lit = "null"
      | Number n <- lit = show n
      | Str s <- lit = s
      | Identifier _ <- lit = "null"
    lit = literal token

