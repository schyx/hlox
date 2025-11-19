module Error (showError, Error (..)) where

import Text.Printf
import Tokens

data Error
  = ScanError Int String String
  | ParseError Token String
  | ResolveError Token String
  | InterpretError Token String

showError :: Error -> String
showError (ScanError scannerLine location message) = report scannerLine location message
showError (ParseError token message)
  | tokenType token == EOF = report (line token) " at end" message
  | otherwise = report (line token) (" at '" ++ lexeme token ++ "'") message
showError (ResolveError token message)
  | tokenType token == EOF = report (line token) " at end" message
  | otherwise = report (line token) (" at '" ++ lexeme token ++ "'") message
showError (InterpretError token message) = printf "%s\n[line %d]" message $ line token

report :: Int -> String -> String -> String
report = printf "[line %d] Error%s: %s"
