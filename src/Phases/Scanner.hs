module Phases.Scanner (scanTokens, ScanResult) where

import Data.Char
import qualified Data.Map as Map
import Error
import Tokens

type ScanResult = Either [String] [Token]

-- | Implements scanning phase of the interpreter
scanTokens :: String -> ScanResult
scanTokens contents =
  case scanHelper contents 1 1 $ Right [] of
    -- reverse the list because we build it up in reverse order
    Left errs -> Left $ reverse errs
    Right tokens -> Right $ reverse tokens

scanHelper :: String -> Int -> Int -> ScanResult -> ScanResult
scanHelper [] currentLine currentOffset currentTokens =
  addToResult currentTokens $
    Right
      MkToken
        { tokenType = EOF,
          offset = currentOffset,
          literal = None,
          line = currentLine,
          lexeme = ""
        }
scanHelper (c : rest) currentLine currentOffset currentTokens
  -- one char tokens
  | c `elem` "(){},.-+;*" =
      scanHelper rest currentLine (currentOffset + 1) $
        addOneCharToken c currentTokens currentOffset currentLine
  -- whitespace
  | c `elem` " \t\r" =
      scanHelper rest currentLine (currentOffset + 1) currentTokens
  -- potentially two char tokens (helper function here?)
  | c == '\n' =
      scanHelper rest (currentLine + 1) 1 currentTokens
  | c `elem` "!=><" =
      let (ttype, newStr, tokenLen, tokenLexeme) = addPotentialTwoCharToken c rest
       in scanHelper
            newStr
            currentLine
            (currentOffset + tokenLen)
            ( addToResult currentTokens $
                Right
                  MkToken
                    { tokenType = ttype,
                      offset = currentOffset,
                      literal = None,
                      line = currentLine,
                      lexeme = tokenLexeme
                    }
            )
  -- comments and divide
  | c == '/' = case rest of
      '/' : _ ->
        scanHelper (removeUntilNewline rest) (currentLine + 1) 1 currentTokens
      '*' : _ ->
        let output = scanBlockComment (c : rest) currentLine currentOffset
         in case output of
              Right (newLine, newOffset, leftovers) -> scanHelper leftovers newLine newOffset currentTokens
              Left err -> addToResult currentTokens $ Left err
      _ ->
        scanHelper rest currentLine (currentOffset + 1) $
          addToResult currentTokens $
            Right
              MkToken
                { tokenType = SLASH,
                  offset = currentOffset,
                  literal = None,
                  line = currentLine,
                  lexeme = "/"
                }
  -- numbers
  | isDigit c =
      let (num, leftovers, numLength, numLexeme) = scanNumber c rest
       in scanHelper leftovers currentLine (currentOffset + numLength) $
            addToResult currentTokens $
              Right
                MkToken
                  { tokenType = NUMBER,
                    offset = currentOffset,
                    literal = Number num,
                    line = currentLine,
                    lexeme = numLexeme
                  }
  -- strings
  | c == '"' =
      let tokenOrErr = scanString rest currentLine currentOffset
       in case tokenOrErr of
            Left err ->
              -- only occurs at end of string, so no need for line/offset checks
              addToResult currentTokens $ Left err
            Right (token, leftovers, newLine, newOffset) ->
              scanHelper leftovers newLine newOffset $ addToResult currentTokens $ Right token
  -- identifiers
  | isAlpha c || c == '_' =
      let (token, leftovers, identifierLength) = scanIdentifier c currentLine currentOffset rest
       in scanHelper leftovers currentLine (currentOffset + identifierLength) $
            addToResult currentTokens $
              Right token
  -- unknown characters
  | otherwise =
      scanHelper rest currentLine (currentOffset + 1) $
        addToResult currentTokens $
          Left $
            report currentLine "" "Unexpected character."

scanBlockComment :: String -> Int -> Int -> Either String (Int, Int, String)
scanBlockComment rest currentLine currentOffset = helper rest currentLine currentOffset 0
  where
    helper :: String -> Int -> Int -> Int -> Either String (Int, Int, String)
    helper ('/' : '*' : leftovers) helperLine helperOffset numOpening =
      helper leftovers helperLine (helperOffset + 2) (numOpening + 1)
    helper ('*' : '/' : leftovers) helperLine helperOffset numOpening =
      if numOpening == 1
        then Right (helperLine, helperOffset + 2, leftovers)
        else helper leftovers helperLine (helperOffset + 2) (numOpening - 1)
    helper ('\n' : c : leftovers) helperLine _ numOpening =
      helper (c : leftovers) (helperLine + 1) 1 numOpening
    helper (_ : c : leftovers) helperLine helperOffset numOpening =
      helper (c : leftovers) helperLine (helperOffset + 1) numOpening
    helper _ helperLine _ _ = Left $ report helperLine "" "Unterminated block comment."

scanString :: String -> Int -> Int -> Either String (Token, String, Int, Int)
scanString rest startLine startOffset = helper [] rest startLine $ startOffset + 1
  where
    helper :: String -> String -> Int -> Int -> Either String (Token, String, Int, Int)
    helper _ [] helperLine _ = Left $ report helperLine "" "Unterminated string."
    helper buildup ('"' : leftovers) helperLine helperOffset =
      getRightReturn buildup leftovers helperLine $ helperOffset + 1 -- account for "
    helper buildup ('\n' : leftovers) helperLine _ =
      helper ('\n' : buildup) leftovers (helperLine + 1) 1
    helper buildup (currentChar : leftovers) helperLine helperOffset =
      helper (currentChar : buildup) leftovers helperLine (helperOffset + 1)
    getRightReturn buildup leftovers helperLine helperOffset =
      let str = reverse buildup
       in Right
            ( MkToken
                { tokenType = STRING,
                  offset = startOffset,
                  literal = Str str,
                  line = startLine,
                  lexeme = "\"" ++ str ++ "\""
                },
              leftovers,
              helperLine,
              helperOffset
            )

scanIdentifier :: Char -> Int -> Int -> String -> (Token, String, Int)
scanIdentifier c identifierLine identifierOffset = helper [c]
  where
    helper :: String -> String -> (Token, String, Int)
    helper buildup [] = getReturn buildup []
    helper buildup (currentChar : leftovers) =
      if isAlphaNum currentChar || currentChar == '_'
        then helper (currentChar : buildup) leftovers
        else getReturn buildup (currentChar : leftovers)
    getReturn buildup leftovers =
      let identifier = reverse buildup
       in case Map.lookup identifier identifierTable of
            Nothing ->
              ( MkToken
                  { tokenType = IDENTIFIER,
                    offset = identifierOffset,
                    literal = Identifier identifier,
                    line = identifierLine,
                    lexeme = identifier
                  },
                leftovers,
                length buildup
              )
            Just NIL ->
              ( MkToken
                  { tokenType = NIL,
                    offset = identifierOffset,
                    literal = Nil,
                    line = identifierLine,
                    lexeme = identifier
                  },
                leftovers,
                length buildup
              )
            Just TRUE ->
              ( MkToken
                  { tokenType = TRUE,
                    offset = identifierOffset,
                    literal = Boolean True,
                    line = identifierLine,
                    lexeme = identifier
                  },
                leftovers,
                length buildup
              )
            Just FALSE ->
              ( MkToken
                  { tokenType = FALSE,
                    offset = identifierOffset,
                    literal = Boolean False,
                    line = identifierLine,
                    lexeme = identifier
                  },
                leftovers,
                length buildup
              )
            Just ttype ->
              ( MkToken
                  { tokenType = ttype,
                    offset = identifierOffset,
                    literal = None,
                    line = identifierLine,
                    lexeme = identifier
                  },
                leftovers,
                length buildup
              )
    identifierTable =
      Map.fromList
        [ ("and", AND),
          ("class", CLASS),
          ("else", ELSE),
          ("false", FALSE),
          ("fun", FUN),
          ("for", FOR),
          ("if", IF),
          ("nil", NIL),
          ("or", OR),
          ("print", PRINT),
          ("return", RETURN),
          ("super", SUPER),
          ("this", THIS),
          ("true", TRUE),
          ("var", VAR),
          ("while", WHILE)
        ]

scanNumber :: Char -> String -> (Double, String, Int, String)
scanNumber c = helper [c] False
  where
    helper :: String -> Bool -> String -> (Double, String, Int, String)
    helper buildup _ [] = getReturn buildup []
    helper buildup True ('.' : leftovers) = getReturn buildup ('.' : leftovers)
    helper buildup False ['.'] = getReturn buildup ['.']
    helper buildup False ('.' : c2 : leftovers)
      | isDigit c2 = helper ('.' : buildup) True (c2 : leftovers)
      | otherwise = getReturn buildup ('.' : leftovers)
    helper buildup seenDot (currentChar : leftovers) =
      if isDigit currentChar
        then helper (currentChar : buildup) seenDot leftovers
        else getReturn buildup (currentChar : leftovers)
    getReturn buildup leftovers =
      let numString = reverse buildup
       in (read numString :: Double, leftovers, length buildup, numString)

addPotentialTwoCharToken :: Char -> String -> (TokenType, String, Int, String)
addPotentialTwoCharToken c rest
  | (c2 : leftovers) <- rest, c == '!' && c2 == '=' = (BANG_EQUAL, leftovers, 2, "!=")
  | (c2 : leftovers) <- rest, c == '=' && c2 == '=' = (EQUAL_EQUAL, leftovers, 2, "==")
  | (c2 : leftovers) <- rest, c == '>' && c2 == '=' = (GREATER_EQUAL, leftovers, 2, ">=")
  | (c2 : leftovers) <- rest, c == '<' && c2 == '=' = (LESS_EQUAL, leftovers, 2, "<=")
  | otherwise = singleToken c
  where
    singleToken char =
      case Map.lookup char table of
        Nothing -> error "bruh"
        Just constructor -> (constructor, rest, 1, [char])

removeUntilNewline :: String -> String
removeUntilNewline [] = []
removeUntilNewline ('\n' : rest) = rest
removeUntilNewline (_ : rest) = removeUntilNewline rest

addOneCharToken :: Char -> ScanResult -> Int -> Int -> ScanResult
addOneCharToken char sr tokenOffset tokenLine =
  case Map.lookup char table of
    Nothing -> error "you called this function incorrectly"
    Just constructor ->
      addToResult
        sr
        $ Right
        $ MkToken
          { tokenType = constructor,
            offset = tokenOffset,
            literal = None,
            line = tokenLine,
            lexeme = [char]
          }

table :: Map.Map Char TokenType
table =
  Map.fromList
    [ ('(', LEFT_PAREN),
      (')', RIGHT_PAREN),
      ('{', LEFT_BRACE),
      ('}', RIGHT_BRACE),
      (',', COMMA),
      ('.', DOT),
      ('-', MINUS),
      ('+', PLUS),
      (';', SEMICOLON),
      ('*', STAR),
      ('!', BANG),
      ('=', EQUAL),
      ('>', GREATER),
      ('<', LESS)
    ]

addToResult :: ScanResult -> Either String Token -> ScanResult
addToResult (Right tokens) (Right token) = Right $ token : tokens
addToResult (Left errs) (Left err) = Left $ err : errs
addToResult (Right _) (Left err) = Left [err]
addToResult (Left errs) (Right _) = Left errs
