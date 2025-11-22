module Main (main) where

import qualified Data.Map as Map
import Error
import Phases.Interpreter
import Phases.Parse
import Phases.Resolver
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
checkArgs [] = startShell
checkArgs [file] = runFile file
checkArgs _ = do
  putStrLn "Usage: hlox [script]"
  exitWith $ ExitFailure 64

startShell :: IO ()
startShell = go $ defaultInterpreter $ Locals Map.empty
 where
  go :: Interpreter -> IO ()
  go interpreter = do
    putStr "> "
    hFlush stdout
    input <- getLine
    modifiedInterpreter <- runInput input interpreter
    go modifiedInterpreter
  runInput :: String -> Interpreter -> IO Interpreter
  runInput input interpreter = do
    let (scanErrs, tokens) = scanTokens input
    if null scanErrs
      then case parse tokens of
        Right stmts -> do
          case resolve stmts of
            Left resolveErrs -> do
              toStderr resolveErrs
              return interpreter
            Right _ -> do -- TODO: add in locals fix
              statementOutput <- runStatements interpreter stmts
              case statementOutput of
                Left interpreterError -> toStderr [interpreterError] >> return interpreter
                Right interpreter' -> return interpreter'
        Left parseErrs -> do
          toStderr parseErrs
          return interpreter
      else toStderr scanErrs >> return interpreter

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
            errOrEnv <- runStatements (defaultInterpreter locals) stmts
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

runStatements :: Interpreter -> [SomeStmt] -> IO (Either Error Interpreter)
runStatements e (s : rest) = do
  newEnvOrErr <- runInterp e s
  case newEnvOrErr of
    Left err -> return $ Left err
    Right newEnv -> runStatements newEnv rest
runStatements e [] = return $ Right e

toStderr :: [Error] -> IO ()
toStderr [] = return ()
toStderr (err : errs) = do
  hPutStrLn stderr $ showError err
  toStderr errs
