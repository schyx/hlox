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
  = ScanOrParseErr [String]
  | RuntimeErr String

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
      errOrEnv <- runStatements env input
      case errOrEnv of
        Right newEnv -> go newEnv
        Left _ -> do
          let scanResult = scanTokens input
          case scanResult of
            Left errs -> do
              toStderr errs
              go env
            Right tokens -> case expression tokens of
              Left (err, _) -> do
                toStderr [err]
                go env
              Right (expr, [_eof]) -> case interpretExpr env expr of
                (Left err, _) -> toStderr [err] >> go env
                (Right lit, _) -> print lit >> go env
              Right (_, _) -> toStderr ["Too many tokens."] >> go env

-- | Runs a Lox file
runFile :: String -> IO ()
runFile filepath = do
  contents <- readFile filepath
  errOrEnv <- runStatements defaultEnvironment contents
  case errOrEnv of
    Right _ -> return ()
    Left (ScanOrParseErr errs) -> do
      toStderr errs
      exitWith $ ExitFailure 65
    Left (RuntimeErr err) -> do
      toStderr [err]
      exitWith $ ExitFailure 70

runStatements :: Environment -> String -> IO (Either Errs Environment)
runStatements env contents = do
  let scanResult = scanTokens contents
  case scanResult of
    Right tokens -> case parse tokens of
      Right stmts -> do
        envOrErr <- go env stmts
        case envOrErr of
          Right newEnv -> return $ Right newEnv
          Left err -> return $ Left $ RuntimeErr err
      Left errs -> do
        return $ Left $ ScanOrParseErr errs
    Left errs -> do
      return $ Left $ ScanOrParseErr errs
  where
    go :: Environment -> [Stmt] -> IO (Either String Environment)
    go e (s : rest) = do
      newEnvOrErr <- runInterp e s
      case newEnvOrErr of
        Left err -> return $ Left err
        Right newEnv -> go newEnv rest
    go e [] = return $ Right e

toStderr :: [String] -> IO ()
toStderr [] = return ()
toStderr (err : errs) = do
  hPutStrLn stderr err
  toStderr errs

runInterp :: Environment -> Stmt -> IO (Either String Environment)
runInterp env s = do
  (newEnv, ok) <- interpret env s
  case ok of
    Right () -> return $ Right newEnv
    Left err -> do
      hPutStrLn stderr err
      return $ Left err
