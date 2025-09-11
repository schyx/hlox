module Main (main) where

import Phases.Interpreter
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
  let scanResult = scanTokens contents
  case scanResult of
    Right tokens -> case parse tokens of
      Right expr -> case interpret expr of
        Right lit -> putStrLn $ printLiteral lit
        Left err -> putStrLn err
      Left errs -> mapM_ putStrLn errs
    Left errs -> mapM_ putStrLn errs

toTestOutput :: Token -> String
toTestOutput token = show (tokenType token) ++ " " ++ lexeme token ++ " " ++ l
  where
    l
      | None <- lit = "null"
      | Number n <- lit = show n
      | Str s <- lit = s
      | Identifier _ <- lit = "null"
      | otherwise = error "not yet implemented"
    lit = literal token

printLiteral :: Literal -> String
printLiteral Nil = "nil"
printLiteral (Number n) = formatNumber n
printLiteral (Str s) = s
printLiteral (Boolean b) = show b
printLiteral _ = error "not yet implemented"

formatNumber :: Double -> String
formatNumber x
  | x == fromInteger (round x) = show (round x :: Integer)
  | otherwise = show x
