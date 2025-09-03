module Token
  ( TokenType,
    Literal,
    Token,
  )
where

data Literal = Number Double | Str String | Identifier String | None deriving (Eq, Show)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    literal :: Literal,
    line :: Int
  }
  deriving (Eq, Show)

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
  | SLASH
  | STAR
  | BANG
  -- Two char tokens
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  -- Variable number char tokens
  | IDENTIFIER
  | STRING
  | NUMBER
  -- Keywords
  | AND
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
  deriving (Show, Eq)
