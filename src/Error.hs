module Error (parseError, report) where

import Text.Printf
import Tokens

parseError :: Token -> String -> String
parseError token message
  | tokenType token == EOF = report (line token) " at end" message
  | otherwise = report (line token) (" at " ++ lexeme token) message

report :: Int -> String -> String -> String
report = printf "[line %d] Error%s: %s"
