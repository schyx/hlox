{-# LANGUAGE LambdaCase #-}

module Phases.Scanner (scanTokens, ScanResult) where

import Control.Applicative (Alternative (many, (<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Error (report)
import Parser (Parser (Parser, runParser))
import Tokens

data ScannerData = ScannerData {restOfInput :: String, getScannerLine :: Int, getScannerOffset :: Int}
  deriving (Show)

lineAndOffset :: ScannerData -> (Int, Int)
lineAndOffset scannerData = (getScannerLine scannerData, getScannerOffset scannerData)

increment :: ScannerData -> ScannerData
increment scannerData =
  scannerData
    { restOfInput = tail $ restOfInput scannerData
    , getScannerOffset = getScannerOffset scannerData + 1
    }

scanNewline :: ScannerData -> ScannerData
scanNewline scannerData =
  scannerData
    { restOfInput = tail $ restOfInput scannerData
    , getScannerOffset = 1
    , getScannerLine = getScannerLine scannerData + 1
    }

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
    inputLines
    $ snd scannerData
 where
  scannerData =
    fromMaybe
      undefined
      $ runLoxScanner (many scanner) (ScannerData{restOfInput = contents, getScannerLine = 1, getScannerOffset = 1})
  inputLines = getScannerLine . fst $ scannerData
  addResult (errors, tokens) eofLine [] =
    ( reverse errors
    , reverse $ MkToken{tokenType = EOF, offset = 1, literal = Nothing, line = eofLine, lexeme = ""} : tokens
    )
  addResult (errors, tokens) eofLine ((Left err) : others) = addResult (err : errors, tokens) eofLine others
  addResult (errors, tokens) eofLine ((Right token) : others) = addResult (errors, token : tokens) eofLine others

scanner :: LoxScanner (Either String Token)
scanner =
  ignore
    *> (identifierScanner <|> numberScanner <|> twoCharTokenScanner <|> singleCharToken <|> stringScanner <|> unknownCharacterScanner)
    <* ignore

ignore :: LoxScanner [(String, (Int, Int))]
ignore = many (whitespace <|> commentScanner <|> newline)

whitespace :: LoxScanner (String, (Int, Int))
whitespace = spanSNoEmpty (`elem` " \t\r")

newline :: LoxScanner (String, (Int, Int))
newline = spanSNoEmpty (== '\n')

identifierScanner :: LoxScanner (Either String Token)
identifierScanner =
  toIdentifier
    <$> ( combineStringMetadata
            <$> oneCharScanner (\c -> isAlpha c || c == '_')
            <*> spanScanner (\c -> isAlphaNum c || c == '_')
        )
 where
  toIdentifier :: (String, (Int, Int)) -> Either String Token
  toIdentifier =
    createToken
      (\string -> fromMaybe IDENTIFIER $ Map.lookup string identifierTable)
      ( \case
          "true" -> Just . Boolean $ True
          "false" -> Just . Boolean $ False
          "nil" -> Just Nil
          _ -> Nothing
      )
      id

stringScanner :: LoxScanner (Either String Token)
stringScanner =
  charScanner (== '"')
    *> ( flip ($)
          <$> spanScanner (/= '"')
          <*> (toStringIdentifier <$ charScanner (== '"') <|> unterminatedString)
       )
 where
  toStringIdentifier = createToken (const STRING) (Just . Str) (\string -> "\"" ++ string ++ "\"")
  unterminatedString = makeLoxScanner $ \inData ->
    Just (inData, const (Left $ report (getScannerLine inData) "" "Unterminated string."))

numberScanner :: LoxScanner (Either String Token)
numberScanner =
  createToken (const NUMBER) (Just . Number . read) id
    <$> ( ( combineThreeStrings
              <$> spanSNoEmpty isDigit
              <*> oneCharScanner (== '.')
              <*> spanSNoEmpty isDigit
          )
            <|> spanSNoEmpty isDigit
        )
 where
  combineThreeStrings a b c = combineStringMetadata a $ combineStringMetadata b c

singleCharToken :: LoxScanner (Either String Token)
singleCharToken = addSingleCharToken <$> charScanner (`elem` "(){},.-+;*!>=</")
 where
  addSingleCharToken (char, (charLine, charOffset)) =
    Right
      MkToken
        { tokenType = singleCharTokenTable Map.! char
        , offset = charOffset
        , literal = Nothing
        , line = charLine
        , lexeme = [char]
        }

twoCharTokenScanner :: LoxScanner (Either String Token)
twoCharTokenScanner = addTwoCharToken <$> twoCharScanner (`elem` twoCharTokens)
 where
  addTwoCharToken = createToken (twoCharTokenTable Map.!) (const Nothing) id
  twoCharTokens = [('!', '='), ('=', '='), ('<', '='), ('>', '=')]

commentScanner :: LoxScanner (String, (Int, Int))
commentScanner = twoCharScanner (== ('/', '/')) <* spanScanner (/= '\n')

unknownCharacterScanner :: LoxScanner (Either String Token)
unknownCharacterScanner = makeLoxScanner $ \inData ->
  if null $ restOfInput inData
    then Nothing
    else
      Just
        ( increment inData
        , Left $ report (getScannerLine inData) "" "Unexpected character."
        )

twoCharScanner :: ((Char, Char) -> Bool) -> LoxScanner (String, (Int, Int))
twoCharScanner predicate = makeLoxScanner $ \inData ->
  case restOfInput inData of
    id1 : id2 : _
      | predicate (id1, id2) -> Just (increment $ increment inData, ([id1, id2], lineAndOffset inData))
      | otherwise -> Nothing
    _ -> Nothing

charScanner :: (Char -> Bool) -> LoxScanner (Char, (Int, Int))
charScanner predicate = makeLoxScanner f
 where
  f inData = case listToMaybe $ restOfInput inData of
    Nothing -> Nothing
    Just c
      | predicate c -> Just (increment inData, (c, lineAndOffset inData))
      | otherwise -> Nothing

oneCharScanner :: (Char -> Bool) -> LoxScanner (String, (Int, Int))
oneCharScanner predicate = fstToString <$> charScanner predicate
 where
  fstToString (c, x) = ([c], x)

combineStringMetadata :: (String, (Int, Int)) -> (String, (Int, Int)) -> (String, (Int, Int))
combineStringMetadata (s1, x) (s2, _) = (s1 ++ s2, x)

spanScanner :: (Char -> Bool) -> LoxScanner (String, (Int, Int))
spanScanner predicate = splitScanner $ span predicate

spanSNoEmpty :: (Char -> Bool) -> LoxScanner (String, (Int, Int))
spanSNoEmpty predicate = splitScannerNoEmpty $ span predicate

splitScanner :: (String -> (String, String)) -> LoxScanner (String, (Int, Int))
splitScanner predicate =
  makeLoxScanner $ \inData ->
    let (string, _) = predicate $ restOfInput inData
        step oldData [] = oldData
        step oldData ('\n' : rest) = step (scanNewline oldData) rest
        step oldData (_ : rest) = step (increment oldData) rest
        outData = step inData string
     in Just (outData, (string, lineAndOffset inData))

splitScannerNoEmpty :: (String -> (String, String)) -> LoxScanner (String, (Int, Int))
splitScannerNoEmpty predicate =
  makeLoxScanner $ \inData ->
    let (string, _) = predicate $ restOfInput inData
        step oldData [] = oldData
        step oldData ('\n' : rest) = step (scanNewline oldData) rest
        step oldData (_ : rest) = step (increment oldData) rest
        outData = step inData string
     in if null string then Nothing else Just (outData, (string, lineAndOffset inData))

createToken ::
  (String -> TokenType) ->
  (String -> Maybe Literal) ->
  (String -> String) ->
  (String, (Int, Int)) ->
  Either String Token
createToken toTokenType toLiteral toLexeme (string, (scannerLine, scannerOffset)) =
  Right $
    MkToken
      { tokenType = toTokenType string
      , offset = scannerOffset
      , literal = toLiteral string
      , line = scannerLine
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
