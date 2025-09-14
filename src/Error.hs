module Error (parseError, runtimeError, report) where

import Text.Printf
import Tokens

parseError :: Token -> String -> String
parseError token message
  | tokenType token == EOF = report (line token) " at end" message
  | otherwise = report (line token) (" at '" ++ lexeme token ++ "'") message

runtimeError :: Token -> String -> String
runtimeError badToken message = printf "%s\n[line %d]" message $ line badToken

report :: Int -> String -> String -> String
report = printf "[line %d] Error%s: %s"
