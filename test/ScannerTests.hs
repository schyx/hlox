module ScannerTests (scannerTests) where

import Phases.Scanner
import Test.HUnit
import Tokens

scannerTests :: Test
scannerTests =
  TestList
    [ testSingleCharToken,
      testBangEqual,
      testMultiCharacter,
      testKeyword,
      testMix,
      testComment,
      testUnterminatedString,
      testUnknownChar,
      testMultipleScanErrors
    ]

scanFile :: String -> IO ScanResult
scanFile file = do
  contents <- readFile $ "test/scannerInputs/" ++ file
  return $ scanTokens contents

testSingleCharToken :: Test
testSingleCharToken = TestCase $ do
  tokens <- scanFile "singleCharToken"
  assertEqual
    "Test single character token"
    ( Right
        [ MkToken {tokenType = SEMICOLON, lexeme = ";", literal = None, line = 1, offset = 1},
          MkToken {tokenType = EOF, offset = 1, literal = None, line = 2, lexeme = ""}
        ]
    )
    tokens

testBangEqual :: Test
testBangEqual = TestCase $ do
  tokens <- scanFile "bangEqual"
  assertEqual
    "test bang equal"
    ( Right
        [ MkToken {tokenType = BANG_EQUAL, lexeme = "!=", literal = None, line = 1, offset = 1},
          MkToken {tokenType = EOF, offset = 1, literal = None, line = 2, lexeme = ""}
        ]
    )
    tokens

testMultiCharacter :: Test
testMultiCharacter = TestCase $ do
  tokens <- scanFile "multiCharacter"
  assertEqual
    "test multiCharacter"
    ( Right
        [ MkToken {tokenType = STRING, lexeme = "hello, world!", literal = Str "hello, world!", line = 1, offset = 1},
          MkToken {tokenType = IDENTIFIER, lexeme = "hello", literal = Identifier "hello", line = 2, offset = 1},
          MkToken {tokenType = NUMBER, lexeme = "123.456", literal = Number 123.456, line = 3, offset = 1},
          MkToken {tokenType = EOF, offset = 1, literal = None, line = 4, lexeme = ""}
        ]
    )
    tokens

testKeyword :: Test
testKeyword = TestCase $ do
  tokens <- scanFile "keyword"
  assertEqual
    "test keyword"
    ( Right
        [ MkToken {tokenType = AND, lexeme = "and", literal = None, line = 1, offset = 1},
          MkToken {tokenType = IF, lexeme = "if", literal = None, line = 2, offset = 1},
          MkToken {tokenType = ELSE, lexeme = "else", literal = None, line = 3, offset = 1},
          MkToken {tokenType = EOF, offset = 1, literal = None, line = 4, lexeme = ""}
        ]
    )
    tokens

testMix :: Test
testMix = TestCase $ do
  tokens <- scanFile "mix"
  assertEqual
    "test mix of tokens"
    ( Right
        [ MkToken {tokenType = LEFT_PAREN, offset = 1, literal = None, line = 1, lexeme = "("},
          MkToken {tokenType = NUMBER, offset = 2, literal = Number 123.456, line = 1, lexeme = "123.456"},
          MkToken {tokenType = RIGHT_PAREN, offset = 9, literal = None, line = 1, lexeme = ")"},
          MkToken {tokenType = SEMICOLON, offset = 10, literal = None, line = 1, lexeme = ";"},
          MkToken {tokenType = AND, offset = 1, literal = None, line = 2, lexeme = "and"},
          MkToken {tokenType = IDENTIFIER, offset = 5, literal = Identifier "andy", line = 2, lexeme = "andy"},
          MkToken {tokenType = SEMICOLON, offset = 9, literal = None, line = 2, lexeme = ";"},
          MkToken {tokenType = PRINT, offset = 1, literal = None, line = 3, lexeme = "print"},
          MkToken {tokenType = STRING, offset = 7, literal = Str "hello", line = 3, lexeme = "hello"},
          MkToken {tokenType = SEMICOLON, offset = 14, literal = None, line = 3, lexeme = ";"},
          MkToken {tokenType = EOF, offset = 1, literal = None, line = 4, lexeme = ""}
        ]
    )
    tokens

testComment :: Test
testComment = TestCase $ do
  tokens <- scanFile "comment"
  assertEqual
    "test comment"
    ( Right
        [ MkToken {tokenType = SEMICOLON, lexeme = ";", literal = None, line = 1, offset = 1},
          MkToken {tokenType = EOF, offset = 1, literal = None, line = 3, lexeme = ""}
        ]
    )
    tokens

testUnterminatedString :: Test
testUnterminatedString = TestCase $ do
  tokens <- scanFile "unterminatedString"
  assertEqual
    "test unterminated string"
    (Left ["[line 2] Error: Unterminated string."])
    tokens

testUnknownChar :: Test
testUnknownChar = TestCase $ do
  tokens <- scanFile "unknownChar"
  assertEqual
    "test unknown char"
    (Left ["[line 1] Error: Unexpected character."])
    tokens

testMultipleScanErrors :: Test
testMultipleScanErrors = TestCase $ do
  tokens <- scanFile "multipleErrs"
  assertEqual
    "test many scanner errors"
    ( Left
        [ "[line 1] Error: Unexpected character.",
          "[line 1] Error: Unexpected character.",
          "[line 3] Error: Unterminated string."
        ]
    )
    tokens
