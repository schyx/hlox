module Main (main) where

import Phases.Environment
import Phases.Interpreter
import Phases.Parser
import Phases.Scanner
import Phases.Stmt
import System.Environment
import System.Exit
import System.IO

data Errs
  = ScanOrParseErr
  | RuntimeErr

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
runPrompt = go defaultEnvironment
  where
    go env = do
      putStr "> "
      hFlush stdout -- required to get the `>` to show up
      input <- getLine
      errOrEnv <- runWithEnv env input
      case errOrEnv of
        Right newEnv -> go newEnv
        Left _ -> go env

-- | Runs a Lox file
runFile :: String -> IO ()
runFile filepath = do
  contents <- readFile filepath
  errOrEnv <- runWithEnv defaultEnvironment contents
  case errOrEnv of
    Right _ -> return ()
    Left ScanOrParseErr -> exitWith $ ExitFailure 65
    Left RuntimeErr -> exitWith $ ExitFailure 70

runWithEnv :: Environment -> String -> IO (Either Errs Environment)
runWithEnv env contents = do
  let scanResult = scanTokens contents
  case scanResult of
    Right tokens -> case parse tokens of
      Right stmts -> do
        envOrErr <- go env stmts
        case envOrErr of
          Right newEnv -> return $ Right newEnv
          Left _ -> return $ Left RuntimeErr
      Left errs -> do
        toStderr errs
        return $ Left ScanOrParseErr
    Left errs -> do
      toStderr errs
      return $ Left ScanOrParseErr
  where
    toStderr :: [String] -> IO ()
    toStderr [] = return ()
    toStderr (err : errs) = do
      hPutStrLn stderr err
      toStderr errs
    go :: Environment -> [Stmt] -> IO (Either String Environment)
    go e (s : rest) = do
      newEnvOrErr <- runInterp e s
      case newEnvOrErr of
        Left err -> return $ Left err
        Right newEnv -> go newEnv rest
    go e [] = return $ Right e

runInterp :: Environment -> Stmt -> IO (Either String Environment)
runInterp env s = do
  (newEnv, ok) <- interpret env s
  case ok of
    Right () -> return $ Right newEnv
    Left err -> do
      hPutStrLn stderr err
      return $ Left err
