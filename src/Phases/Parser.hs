module Phases.Parser (parse, ParseResult) where

import Error
import Phases.Expr
import Phases.Stmt
import Tokens

type ParseResult = Either [String] [Stmt]

type ParseExpressionResult = (Either String Expr, [Token])

type ParseStatementResult = (Either String Stmt, [Token])

parse :: [Token] -> ParseResult
parse input = case parseHelper input $ Right [] of
  Left errs -> Left $ reverse errs
  Right stmts -> Right $ reverse stmts
  where
    parseHelper :: [Token] -> ParseResult -> ParseResult
    parseHelper tokens result =
      let ((output, leftovers), shouldContinue) = statement tokens
       in if shouldContinue
            then parseHelper leftovers $ addToResult result output
            else result

statement :: [Token] -> (ParseStatementResult, Bool)
statement (t : rest)
  | ttype == EOF = ((Left "", []), False)
  | ttype == PRINT = (printStatement $ t : rest, True)
  | otherwise = (expressionStatement $ t : rest, True)
  where
    ttype = tokenType t
statement [] = error "Should have at least EOF in statement"

expression :: [Token] -> ParseExpressionResult
expression = equality

printStatement :: [Token] -> ParseStatementResult
printStatement (_ : rest) = case expression rest of
  (Right expr, t : leftovers) ->
    if tokenType t == SEMICOLON
      then (Right $ Print expr, leftovers)
      else (Left $ parseError t "Expect ';' after value.", leftovers)
  (Left err, leftovers) -> (Left err, leftovers)
  _ -> error "Shouln't have empty tokens in printStatement"
printStatement [] = error "Shouln't have empty tokens in printStatement"

expressionStatement :: [Token] -> ParseStatementResult
expressionStatement ts = case expression ts of
  (Right expr, t : leftovers) ->
    if tokenType t == SEMICOLON
      then (Right $ Expression expr, leftovers)
      else (Left $ parseError t "Expect ';' after value.", leftovers)
  (Left err, leftovers) -> (Left err, leftovers)
  _ -> error "Shouldn't have empty tokens in expressionStatement"

equality :: [Token] -> ParseExpressionResult
equality tokens = case comparison tokens of
  (Left err, leftovers) -> (Left err, leftovers)
  (Right left, token : rest) ->
    if tokenType token `elem` [EQUAL_EQUAL, BANG_EQUAL]
      then equalityHelper left token rest
      else (Right left, token : rest)
  _ -> error "should at least see EOF in equality"
  where
    equalityHelper :: Expr -> Token -> [Token] -> ParseExpressionResult
    equalityHelper left token leftovers = case comparison leftovers of
      (Left err, rest) -> (Left err, rest)
      (Right right, newToken : rest) ->
        if tokenType newToken `elem` [EQUAL_EQUAL, BANG_EQUAL]
          then equalityHelper (Binary left token right) newToken rest
          else (Right $ Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in equality helper"

comparison :: [Token] -> ParseExpressionResult
comparison tokens = case term tokens of
  (Left err, leftovers) -> (Left err, leftovers)
  (Right left, token : rest) ->
    if tokenType token `elem` [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]
      then comparisonHelper left token rest
      else (Right left, token : rest)
  _ -> error "should at least see EOF in comparison"
  where
    comparisonHelper :: Expr -> Token -> [Token] -> ParseExpressionResult
    comparisonHelper left token leftovers = case term leftovers of
      (Left err, rest) -> (Left err, rest)
      (Right right, newToken : rest) ->
        if tokenType newToken `elem` [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]
          then comparisonHelper (Binary left token right) newToken rest
          else (Right $ Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in comparison helper"

term :: [Token] -> ParseExpressionResult
term tokens = case factor tokens of
  (Left err, leftovers) -> (Left err, leftovers)
  (Right left, token : rest) ->
    if tokenType token `elem` [PLUS, MINUS]
      then termHelper left token rest
      else (Right left, token : rest)
  _ -> error "should at least see EOF in term"
  where
    termHelper :: Expr -> Token -> [Token] -> ParseExpressionResult
    termHelper left token leftovers = case factor leftovers of
      (Left err, rest) -> (Left err, rest)
      (Right right, newToken : rest) ->
        if tokenType newToken `elem` [PLUS, MINUS]
          then termHelper (Binary left token right) newToken rest
          else (Right $ Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in term helper"

factor :: [Token] -> ParseExpressionResult
factor tokens = case unary tokens of
  (Left err, leftovers) -> (Left err, leftovers)
  (Right left, token : rest) ->
    if tokenType token `elem` [SLASH, STAR]
      then factorHelper left token rest
      else (Right left, token : rest)
  _ -> error "should at least see EOF in factor"
  where
    factorHelper :: Expr -> Token -> [Token] -> ParseExpressionResult
    factorHelper left token leftovers = case unary leftovers of
      (Left err, rest) -> (Left err, rest)
      (Right right, newToken : rest) ->
        if tokenType newToken `elem` [SLASH, STAR]
          then factorHelper (Binary left token right) newToken rest
          else (Right $ Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in factor helper"

unary :: [Token] -> ParseExpressionResult
unary (token : rest)
  | tokenType token `elem` [MINUS, BANG] =
      case unary rest of
        (Left err, leftovers) -> (Left err, leftovers)
        (Right right, leftovers) -> (Right $ Unary token right, leftovers)
  | otherwise = primary (token : rest)
unary [] = error "should at least see EOF in unary"

primary :: [Token] -> ParseExpressionResult
primary (token : rest)
  | tokenType token `elem` [FALSE, TRUE, NUMBER, STRING, NIL] =
      (Right $ Primary $ literal token, rest)
  | tokenType token == LEFT_PAREN =
      case equality rest of
        (Left err, leftovers) -> (Left err, leftovers)
        (Right inner, next : leftovers) ->
          if tokenType next == RIGHT_PAREN
            then (Right $ Grouping inner, leftovers)
            else (Left $ parseError token "Expect ')' after expression", leftovers)
        _ -> error "should always at least EOF in primary"
  | otherwise = (Left $ parseError token "Expect expression.", rest)
primary _ = error "should always at least EOF in primary"

addToResult :: ParseResult -> Either String Stmt -> ParseResult
addToResult (Right stmts) (Right stmt) = Right $ stmt : stmts
addToResult (Left errs) (Left err) = Left $ err : errs
addToResult (Right _) (Left err) = Left [err]
addToResult errs _ = errs
