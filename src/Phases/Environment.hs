module Phases.Environment (Environment (..), defaultEnvironment, define, get, assign) where

import qualified Data.Map as Map
import Error
import Tokens

data Environment = Environment (Map.Map String Literal)

defaultEnvironment :: Environment
defaultEnvironment = Environment Map.empty

define :: Environment -> Token -> Literal -> Environment
define (Environment table) var val = Environment $ Map.insert (lexeme var) val table

assign :: Environment -> Token -> Literal -> (Either String (), Environment)
assign (Environment table) var val =
  if Map.member (lexeme var) table
    then (Right (), Environment $ Map.insert (lexeme var) val table)
    else (Left $ runtimeError var $ "Undefined variable '" ++ lexeme var ++ "'.", Environment table)

get :: Environment -> Token -> Either String Literal
get (Environment table) var = case Map.lookup (lexeme var) table of
  Just val -> Right val
  Nothing -> Left $ runtimeError var ("Undefined variable '" ++ lexeme var ++ "'.")
