module Tokens (
  TokenType (..),
  Literal (..),
  Token (..),
)
where

import Numeric

data Literal
  = Number Double
  | Str String
  | Boolean Bool
  | Nil
  deriving (Eq, Ord)

instance Show Literal where
  show (Number number)
    | isNegativeZero number = "-0"
    | number == fromInteger (round number) = show (round number :: Integer)
    | otherwise = showFFloat Nothing number ""
  show (Str string) = string
  show (Boolean boolean) = if boolean then "true" else "false"
  show Nil = "nil"

data Token = MkToken
  { tokenType :: TokenType
  , lexeme :: String
  , literal :: Maybe Literal
  , line :: Int
  , offset :: Int
  }
  deriving (Eq, Ord, Show)

data TokenType
  = LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | STAR
  | -- Two char tokens
    SLASH
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Variable number char tokens
    IDENTIFIER
  | STRING
  | NUMBER
  | -- Keywords
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  deriving (Eq, Ord, Show)
