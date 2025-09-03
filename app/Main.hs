module Main (main) where

import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do args <- getArgs
          checkArgs args

checkArgs :: [String] -> IO ()
checkArgs [] = runPrompt
checkArgs [file] = runFile file
checkArgs _ = do putStrLn "Usage: hlox [script]"
                 exitWith $ ExitFailure 64

-- | Creates Lox repl
runPrompt :: IO ()
runPrompt = do putStr "> "
               hFlush stdout -- required to get the `>` to show up
               input <- getLine
               run input
               runPrompt

-- | Runs a Lox file
runFile :: String -> IO ()
runFile filepath = do contents <- readFile filepath
                      run contents

run :: String -> IO ()
run = putStrLn
