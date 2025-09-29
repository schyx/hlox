module Phases.Environment (
  Environment (..),
  defaultEnvironment,
  define, get, assign,
  envWithParent, getParent,
  Value(..), fromLiteral,
) where

import qualified Data.Map as Map
import           Error
import           Numeric
import           Tokens

data Environment =
  Environment
    (Map.Map String Value)
    (Map.Map Value (Environment -> [Value] -> Value))
    (Maybe Environment)

defaultEnvironment :: Environment
defaultEnvironment = Environment Map.empty Map.empty Nothing

envWithParent :: Environment -> Environment
envWithParent = Environment Map.empty Map.empty . Just

getParent :: Environment -> Environment
getParent (Environment _ _ pEnv)
  | Just parent <- pEnv = parent
  | Nothing <- pEnv = error "called this in the wrong place"

define :: Environment -> Token -> Value -> Environment
define (Environment table funcTable parent) var val = Environment (Map.insert (lexeme var) val table) funcTable parent

assign :: Environment -> Token -> Value -> (Either String (), Environment)
assign (Environment table funcTable parent) var val
  | Map.member (lexeme var) table = (Right (), Environment (Map.insert (lexeme var) val table) funcTable parent)
  | (Just pEnv) <- parent = case assign pEnv var val of
      (Left err, newParent) -> (Left err, Environment table funcTable $ Just newParent)
      (Right (), newParent) -> (Right (), Environment table funcTable $ Just newParent)
  | Nothing <- parent = (Left $ runtimeError var $ "Undefined variable '" ++ lexeme var ++ "'.", Environment table funcTable parent)

get :: Environment -> Token -> Either String Value
get (Environment table _ parent) var
  | Just val <- Map.lookup (lexeme var) table = Right val
  | Just pEnv <- parent = get pEnv var
  | Nothing <- parent = Left $ runtimeError var ("Undefined variable '" ++ lexeme var ++ "'.")

data Value
  = VNumber Double
  | VStr String
  | VBoolean Bool
  | VNil
  | VCall Int Token
  deriving (Eq, Ord)

instance Show Value where -- TODO: change literal to not include identifiers
  show (VNumber n) = formatNumber n
    where
      formatNumber :: Double -> String
      formatNumber x
        | isNegativeZero x = "-0"
        | x == fromInteger (round x) = show (round x :: Integer)
        | otherwise = showFFloat Nothing x ""
  show (VStr s) = s
  show (VBoolean b) = if b then "true" else "false"
  show VNil = "nil"

fromLiteral :: Literal -> Value
fromLiteral (Tokens.Number n)  = VNumber n
fromLiteral (Tokens.Str s)     = VStr s
fromLiteral (Tokens.Boolean b) = VBoolean b
fromLiteral Tokens.Nil         = VNil
fromLiteral _                  = error "can't convert from literal"
