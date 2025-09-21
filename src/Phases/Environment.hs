module Phases.Environment (Environment (..), defaultEnvironment, define, get, assign, envWithParent, getParent) where

import qualified Data.Map as Map
import           Error
import           Tokens

data Environment = Environment (Map.Map String Literal) (Maybe Environment)

defaultEnvironment :: Environment
defaultEnvironment = Environment Map.empty Nothing

envWithParent :: Environment -> Environment
envWithParent = Environment Map.empty . Just

getParent :: Environment -> Environment
getParent (Environment _ pEnv)
  | Just parent <- pEnv = parent
  | Nothing <- pEnv = error "called this in the wrong place"

define :: Environment -> Token -> Literal -> Environment
define (Environment table parent) var val = Environment (Map.insert (lexeme var) val table) parent

assign :: Environment -> Token -> Literal -> (Either String (), Environment)
assign (Environment table parent) var val
  | Map.member (lexeme var) table = (Right (), Environment (Map.insert (lexeme var) val table) parent)
  | (Just pEnv) <- parent = case assign pEnv var val of
      (Left err, newParent) -> (Left err, Environment table $ Just newParent)
      (Right (), newParent) -> (Right (), Environment table $ Just newParent)
  | Nothing <- parent = (Left $ runtimeError var $ "Undefined variable '" ++ lexeme var ++ "'.", Environment table parent)

get :: Environment -> Token -> Either String Literal
get (Environment table parent) var
  | Just val <- Map.lookup (lexeme var) table = Right val
  | Just pEnv <- parent = get pEnv var
  | Nothing <- parent = Left $ runtimeError var ("Undefined variable '" ++ lexeme var ++ "'.")
