module Tokens
  ( TokenType (..),
    Literal (..),
    Token (..),
  )
where

import Numeric -- to not get scientific notation

data Literal
  = Number Double
  | Str String
  | Identifier String
  | Boolean Bool
  | Nil
  | None
  deriving (Eq, Ord)

instance Show Literal where -- TODO: change literal to not include identifiers
  show (Number n) = formatNumber n
    where
      formatNumber :: Double -> String
      formatNumber x
        | isNegativeZero x = "-0"
        | x == fromInteger (round x) = show (round x :: Integer)
        | otherwise = showFFloat Nothing x ""
  show (Str s) = s
  show (Identifier _) = error "shouldn't be showing identifier"
  show (Boolean b) = if b then "true" else "false"
  show Nil = "nil"
  show None = error "shouldn't be showing none"

data Token = MkToken
  { tokenType :: TokenType,
    lexeme :: String,
    literal :: Literal,
    line :: Int,
    offset :: Int
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
