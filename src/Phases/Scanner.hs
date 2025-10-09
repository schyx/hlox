{-# LANGUAGE LambdaCase #-}

module Phases.Scanner (scanTokens, ScanResult) where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Error (report)
import Parser (Parser (Parser, runParser))
import Tokens

data ScannerData = ScannerData {restOfInput :: String, scannerLine :: Int, scannerOffset :: Int}
  deriving (Show)

lineAndOffset :: ScannerData -> (Int, Int)
lineAndOffset (ScannerData _ l o) = (l, o)

increment :: ScannerData -> ScannerData
increment sd = sd{restOfInput = tail $ restOfInput sd, scannerOffset = scannerOffset sd + 1}

scanNewline :: ScannerData -> ScannerData
scanNewline sd = sd{restOfInput = tail $ restOfInput sd, scannerOffset = 1, scannerLine = scannerLine sd + 1}

type LoxScanner a = Parser ScannerData a

makeLoxScanner :: (ScannerData -> Maybe (ScannerData, a)) -> LoxScanner a
makeLoxScanner = Parser

runLoxScanner :: LoxScanner a -> (ScannerData -> Maybe (ScannerData, a))
runLoxScanner = runParser

type ScanResult = ([String], [Token])

scanTokens :: String -> ScanResult
scanTokens contents =
  addResult
    ([], [])
    $ snd
    $ fromMaybe undefined
    $ runLoxScanner (many scanner) (ScannerData{restOfInput = contents, scannerLine = 1, scannerOffset = 1})
 where
  addResult (errs, toks) [] =
    ( reverse errs
    , reverse $ MkToken{tokenType = EOF, offset = 1, literal = Nil, line = 1, lexeme = ""} : toks
    )
  addResult (errs, toks) ((Left err) : others) = addResult (err : errs, toks) others
  addResult (errs, toks) ((Right tok) : others) = addResult (errs, tok : toks) others

scanner :: LoxScanner (Either String Token)
scanner =
  ignore
    *> (identifierS <|> numS <|> twoCharToken <|> singleCharToken <|> stringS <|> unknownChars)
    <* ignore

ignore :: LoxScanner [(String, (Int, Int))]
ignore = many (whitespace <|> comment <|> newline)

whitespace :: LoxScanner (String, (Int, Int))
whitespace = spanSNoEmpty (`elem` " \t\r")

newline :: LoxScanner (String, (Int, Int))
newline = spanSNoEmpty (== '\n')

identifierS :: LoxScanner (Either String Token)
identifierS =
  toIdentifier
    <$> ( combineStringMetadata
            <$> oneCharStringS (\c -> isAlpha c || c == '_')
            <*> spanS (\c -> isAlphaNum c || c == '_')
        )
 where
  toIdentifier :: (String, (Int, Int)) -> Either String Token
  toIdentifier =
    createToken
      (\string -> fromMaybe IDENTIFIER $ Map.lookup string identifierTable)
      ( \case
          "true" -> Boolean True
          "false" -> Boolean False
          _ -> Nil
      )
      id

stringS :: LoxScanner (Either String Token)
stringS =
  charS (== '"')
    *> ( flip ($)
          <$> spanS (/= '"')
          <*> (toStringIdentifier <$ charS (== '"') <|> unterminatedString)
       )
 where
  toStringIdentifier = createToken (const STRING) Str (\string -> "\"" ++ string ++ "\"")
  unterminatedString = makeLoxScanner $ \inData ->
    Just (inData, const (Left $ report (scannerLine inData) "" "Unterminated string."))

numS :: LoxScanner (Either String Token)
numS =
  createToken (const NUMBER) (Number . read) id
    <$> ( ( combineThreeStrings
              <$> spanSNoEmpty isDigit
              <*> oneCharStringS (== '.')
              <*> spanSNoEmpty isDigit
          )
            <|> spanSNoEmpty isDigit
        )
 where
  combineThreeStrings a b c = combineStringMetadata a $ combineStringMetadata b c

singleCharToken :: LoxScanner (Either String Token)
singleCharToken = addSingleCharToken <$> charS (`elem` "(){},.-+;*!>=</")
 where
  addSingleCharToken (c, (charLine, charOffset)) =
    Right
      MkToken
        { tokenType = singleCharTokenTable Map.! c
        , offset = charOffset
        , literal = Nil
        , line = charLine
        , lexeme = [c]
        }

twoCharToken :: LoxScanner (Either String Token)
twoCharToken = addTwoCharToken <$> twoCharS (`elem` twoCharTokens)
 where
  addTwoCharToken = createToken (twoCharTokenTable Map.!) (const Nil) id
  twoCharTokens = [('!', '='), ('=', '='), ('<', '='), ('>', '=')]

comment :: LoxScanner (String, (Int, Int))
comment = twoCharS (== ('/', '/')) <* spanS (/= '\n')

unknownChars :: LoxScanner (Either String Token)
unknownChars = makeLoxScanner $ \inData ->
  if null $ restOfInput inData
    then Nothing
    else
      Just
        ( increment inData
        , Left $ report (scannerLine inData) "" "Unexpected character."
        )

twoCharS :: ((Char, Char) -> Bool) -> LoxScanner (String, (Int, Int))
twoCharS predicate = makeLoxScanner $ \inData ->
  case restOfInput inData of
    id1 : id2 : _
      | predicate (id1, id2) -> Just (increment $ increment inData, ([id1, id2], lineAndOffset inData))
      | otherwise -> Nothing
    _ -> Nothing

charS :: (Char -> Bool) -> LoxScanner (Char, (Int, Int))
charS predicate = makeLoxScanner f
 where
  f inData = case listToMaybe $ restOfInput inData of
    Nothing -> Nothing
    Just c
      | predicate c -> Just (increment inData, (c, lineAndOffset inData))
      | otherwise -> Nothing

oneCharStringS :: (Char -> Bool) -> LoxScanner (String, (Int, Int))
oneCharStringS predicate = fstToString <$> charS predicate
 where
  fstToString (c, x) = ([c], x)

combineStringMetadata :: (String, (Int, Int)) -> (String, (Int, Int)) -> (String, (Int, Int))
combineStringMetadata (s1, x) (s2, _) = (s1 ++ s2, x)

spanS :: (Char -> Bool) -> LoxScanner (String, (Int, Int))
spanS predicate = splitS $ span predicate

spanSNoEmpty :: (Char -> Bool) -> LoxScanner (String, (Int, Int))
spanSNoEmpty predicate = splitSNoEmpty $ span predicate

splitS :: (String -> (String, String)) -> LoxScanner (String, (Int, Int))
splitS predicate =
  makeLoxScanner $ \inData ->
    let (string, _) = predicate $ restOfInput inData
        step oldData [] = oldData
        step oldData ('\n' : rest) = step (scanNewline oldData) rest
        step oldData (_ : rest) = step (increment oldData) rest
        outData = step inData string
     in Just (outData, (string, lineAndOffset inData))

splitSNoEmpty :: (String -> (String, String)) -> LoxScanner (String, (Int, Int))
splitSNoEmpty predicate =
  makeLoxScanner $ \inData ->
    let (string, _) = predicate $ restOfInput inData
        step oldData [] = oldData
        step oldData ('\n' : rest) = step (scanNewline oldData) rest
        step oldData (_ : rest) = step (increment oldData) rest
        outData = step inData string
     in if null string then Nothing else Just (outData, (string, lineAndOffset inData))

createToken ::
  (String -> TokenType) ->
  (String -> Literal) ->
  (String -> String) ->
  (String, (Int, Int)) ->
  Either String Token
createToken toTokenType toLiteral toLexeme (string, (l, o)) =
  Right $
    MkToken
      { tokenType = toTokenType string
      , offset = o
      , literal = toLiteral string
      , line = l
      , lexeme = toLexeme string
      }

singleCharTokenTable :: Map.Map Char TokenType
singleCharTokenTable =
  Map.fromList
    [ ('(', LEFT_PAREN)
    , (')', RIGHT_PAREN)
    , ('{', LEFT_BRACE)
    , ('}', RIGHT_BRACE)
    , (',', COMMA)
    , ('.', DOT)
    , ('-', MINUS)
    , ('+', PLUS)
    , (';', SEMICOLON)
    , ('*', STAR)
    , ('!', BANG)
    , ('=', EQUAL)
    , ('>', GREATER)
    , ('<', LESS)
    , ('/', SLASH)
    ]

twoCharTokenTable :: Map.Map String TokenType
twoCharTokenTable =
  Map.fromList [("!=", BANG_EQUAL), ("==", EQUAL_EQUAL), ("<=", LESS_EQUAL), (">=", GREATER_EQUAL)]

identifierTable :: Map.Map String TokenType
identifierTable =
  Map.fromList
    [ ("and", AND)
    , ("class", CLASS)
    , ("else", ELSE)
    , ("false", FALSE)
    , ("fun", FUN)
    , ("for", FOR)
    , ("if", IF)
    , ("nil", NIL)
    , ("or", OR)
    , ("print", PRINT)
    , ("return", RETURN)
    , ("super", SUPER)
    , ("this", THIS)
    , ("true", TRUE)
    , ("var", VAR)
    , ("while", WHILE)
    ]
