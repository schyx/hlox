module Phases.Parser (parse) where

import Error
import Phases.Expr
import Tokens

type ParseResult = Either [String] Expr

type ParseHelperResult = Either [String] (Expr, [Token])

parse :: [Token] -> ParseResult
parse tokens = case equality tokens of
  Left errs -> Left errs
  Right (expr, _) -> Right expr

equality :: [Token] -> ParseHelperResult
equality tokens = case comparison tokens of
  Left errs -> Left errs
  Right (left, token : rest) ->
    if tokenType token `elem` [EQUAL_EQUAL, BANG_EQUAL]
      then equalityHelper left token rest
      else Right (left, token : rest)
  _ -> error "should at least see EOF in equality"
  where
    equalityHelper :: Expr -> Token -> [Token] -> ParseHelperResult
    equalityHelper left token leftovers = case comparison leftovers of
      Left errs -> Left errs
      Right (right, newToken : rest) ->
        if tokenType newToken `elem` [EQUAL_EQUAL, BANG_EQUAL]
          then equalityHelper (Binary left token right) newToken rest
          else Right (Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in equality helper"

comparison :: [Token] -> ParseHelperResult
comparison tokens = case term tokens of
  Left errs -> Left errs
  Right (left, token : rest) ->
    if tokenType token `elem` [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]
      then comparisonHelper left token rest
      else Right (left, token : rest)
  _ -> error "should at least see EOF in comparison"
  where
    comparisonHelper :: Expr -> Token -> [Token] -> ParseHelperResult
    comparisonHelper left token leftovers = case term leftovers of
      Left errs -> Left errs
      Right (right, newToken : rest) ->
        if tokenType newToken `elem` [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]
          then comparisonHelper (Binary left token right) newToken rest
          else Right (Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in comparison helper"

term :: [Token] -> ParseHelperResult
term tokens = case factor tokens of
  Left errs -> Left errs
  Right (left, token : rest) ->
    if tokenType token `elem` [PLUS, MINUS]
      then termHelper left token rest
      else Right (left, token : rest)
  _ -> error "should at least see EOF in term"
  where
    termHelper :: Expr -> Token -> [Token] -> ParseHelperResult
    termHelper left token leftovers = case factor leftovers of
      Left errs -> Left errs
      Right (right, newToken : rest) ->
        if tokenType newToken `elem` [PLUS, MINUS]
          then termHelper (Binary left token right) newToken rest
          else Right (Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in term helper"

factor :: [Token] -> ParseHelperResult
factor tokens = case unary tokens of
  Left errs -> Left errs
  Right (left, token : rest) ->
    if tokenType token `elem` [SLASH, STAR]
      then factorHelper left token rest
      else Right (left, token : rest)
  _ -> error "should at least see EOF in factor"
  where
    factorHelper :: Expr -> Token -> [Token] -> ParseHelperResult
    factorHelper left token leftovers = case unary leftovers of
      Left errs -> Left errs
      Right (right, newToken : rest) ->
        if tokenType newToken `elem` [SLASH, STAR]
          then factorHelper (Binary left token right) newToken rest
          else Right (Binary left token right, newToken : rest)
      _ -> error "should at least see EOF in factor helper"

unary :: [Token] -> ParseHelperResult
unary (token : rest)
  | tokenType token `elem` [MINUS, BANG] =
      case unary rest of
        Left errs -> Left errs
        Right (right, leftovers) -> Right (Unary token right, leftovers)
  | otherwise = primary (token : rest)
unary [] = error "should at least see EOF in unary"

primary :: [Token] -> ParseHelperResult
primary (token : rest)
  | tokenType token `elem` [FALSE, TRUE, NUMBER, STRING, NIL] =
      Right (Primary $ literal token, rest)
  | tokenType token == LEFT_PAREN =
      case equality rest of
        Left errs -> Left errs
        Right (inner, next : leftovers) ->
          if tokenType next == RIGHT_PAREN
            then Right (Grouping inner, leftovers)
            else Left [parseError token "Expect ')' after expression"]
        _ -> Left [parseError token "Expect ')' after expression"]
  | otherwise = Left [parseError token "Expect expression."]
primary _ = error "should always at least EOF in primary"
