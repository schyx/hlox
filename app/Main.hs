module Main (main) where

import Phases.Environment
import Phases.Interpreter
import Phases.Parser
import Phases.Scanner
import Phases.Stmt
import System.Environment
import System.Exit
import System.IO

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
runPrompt :: IO () -- deal with environment persisting
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
      Right stmts -> runInterp defaultEnvironment stmts
      Left errs -> do
        toStderr errs
        exitWith $ ExitFailure 65
    Left errs -> do
      toStderr errs
      exitWith $ ExitFailure 65
  where
    toStderr :: [String] -> IO ()
    toStderr [] = return ()
    toStderr (err : errs) = do
      hPutStrLn stderr err
      toStderr errs
    runInterp :: Environment -> [Stmt] -> IO ()
    runInterp _ [] = return ()
    runInterp env (s : rest) = do
      (newEnv, ok) <- interpret env s
      case ok of
        Right () -> runInterp newEnv rest
        Left err -> do
          hPutStrLn stderr err
          exitWith $ ExitFailure 70
