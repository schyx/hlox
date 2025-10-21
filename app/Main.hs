module Main (main) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map as Map
import Phases.Interpret
import Phases.Interpreter
import Phases.Parse
import Phases.Resolver
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
runPrompt = go (defaultInterpreter $ Locals Map.empty)
 where
  go env = do
    putStr "> "
    hFlush stdout -- required to get the `>` to show up
    input <- getLine
    errOrEnv <- runStatements env input
    case errOrEnv of
      Right newEnv -> go newEnv
      Left _ -> do
        let (errs, tokens) = scanTokens input
        if null errs
          then case expressionWrapper tokens of
            Left err -> do
              toStderr err
              go env
            Right expr -> do
              exprOutput <- runExceptT $ runExceptT $ interpretExpr env expr
              case exprOutput of
                Left err -> toStderr [err] >> go env
                Right (Left _) -> error "got return value?"
                Right (Right (lit, _)) -> print lit >> go env
          else do
            toStderr errs
            go env

-- | Runs a Lox file
runFile :: String -> IO ()
runFile filepath = do
  contents <- readFile filepath
  let (scanErrs, tokens) = scanTokens contents
  if null scanErrs
    then case parse tokens of
      Right stmts -> do
        case resolve stmts of
          Left resolverErrs -> do
            toStderr resolverErrs
            exitWith $ ExitFailure 65
          Right locals -> do
            errOrEnv <- go (defaultInterpreter locals) stmts
            case errOrEnv of
              Right _ -> return ()
              Left _ -> do
                exitWith $ ExitFailure 70
      Left parseErrs -> do
        toStderr $ scanErrs ++ parseErrs
        exitWith $ ExitFailure 65
    else case parse tokens of
      Right _ -> do
        toStderr scanErrs
        exitWith $ ExitFailure 65
      Left parseErrs -> do
        toStderr $ scanErrs ++ parseErrs
        exitWith $ ExitFailure 65
 where
  go :: Interpreter -> [SomeStmt] -> IO (Either String Interpreter)
  go e (s : rest) = do
    newEnvOrErr <- runInterp e s
    case newEnvOrErr of
      Left err -> return $ Left err
      Right newEnv -> go newEnv rest
  go e [] = return $ Right e

runStatements :: Interpreter -> String -> IO (Either Errs Interpreter)
runStatements env contents = do
  let (errs, tokens) = scanTokens contents
  if null errs
    then case parse tokens of
      Right stmts -> do
        envOrErr <- go env stmts
        case envOrErr of
          Right newEnv -> return $ Right newEnv
          Left err -> return $ Left $ RuntimeErr err
      Left parseErrs -> do
        return $ Left $ ScanOrParseErr parseErrs
    else do
      case parse tokens of
        Right _ -> return $ Left $ ScanOrParseErr errs
        Left parseErrs -> return $ Left $ ScanOrParseErr $ errs ++ parseErrs
 where
  go :: Interpreter -> [SomeStmt] -> IO (Either String Interpreter)
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

runInterp :: Interpreter -> SomeStmt -> IO (Either String Interpreter)
runInterp env s = do
  interpOutput <- runExceptT $ runExceptT $ interpret env s
  case interpOutput of
    Right (Right newEnv) -> return $ Right newEnv
    Right (Left _) -> error "got return value"
    Left err -> do
      hPutStrLn stderr err
      return $ Left err
