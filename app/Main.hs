module Main (main) where

import Phases.Interpreter
import Phases.Parse
import Phases.Resolver
import Phases.Scanner
import Phases.Stmt
import System.Environment
import System.Exit
import System.IO

-- TODO: add in repl behavior

main :: IO ()
main = do
  args <- getArgs
  checkArgs args

checkArgs :: [String] -> IO ()
checkArgs [] = undefined
checkArgs [file] = runFile file
checkArgs _ = do
  putStrLn "Usage: hlox [script]"
  exitWith $ ExitFailure 64

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

toStderr :: [String] -> IO ()
toStderr [] = return ()
toStderr (err : errs) = do
  hPutStrLn stderr err
  toStderr errs
