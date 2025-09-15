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
      let ((output, leftovers), shouldContinue) = declaration tokens
       in if shouldContinue
            then parseHelper leftovers $ addToResult result output
            else result

declaration :: [Token] -> (ParseStatementResult, Bool)
declaration (t : rest)
  | ttype == EOF = ((Left "", []), False)
  | ttype == VAR = case varDeclarationStatement $ t : rest of 
      (Right expr, leftovers) -> ((Right expr, leftovers), True)
      (Left err, leftovers) -> ((Left err, synchronize leftovers), True)
  | otherwise = case statement $ t : rest of
      (Right expr, leftovers) -> ((Right expr, leftovers), True)
      (Left err, leftovers) -> ((Left err, synchronize leftovers), True)
  where
    ttype = tokenType t
declaration _ = error "should at least have EOF in declaration"

expression :: [Token] -> ParseExpressionResult
expression = assignment

varDeclarationStatement :: [Token] -> ParseStatementResult
varDeclarationStatement (_ : t1 : afterIdentifier)
  | tokenType t1 == IDENTIFIER =
      case afterIdentifier of
        t2 : afterEqual -> case tokenType t2 of
          EQUAL -> case expression afterEqual of
            (Left err, leftovers) -> (Left err, leftovers)
            (Right expr, l1 : leftovers) ->
              if tokenType l1 == SEMICOLON
                then (Right $ Var t1 expr, leftovers)
                else
                  ( Left $ parseError l1 "Expect ';' after variable declaration.",
                    synchronize $ l1 : leftovers
                  )
            _ -> error "Should at least have EOF in varDeclarationStatement"
          SEMICOLON -> (Right $ Var t1 (Primary Nil), afterEqual)
          _ ->
            ( Left $ parseError t2 "Expect ';' after variable declaration.",
              synchronize $ t2 : afterEqual
            )
        _ -> error "Should at least have EOF in varDeclarationStatement 2"
  | otherwise = (Left $ parseError t1 "Expect variable name.", synchronize $ t1 : afterIdentifier)
varDeclarationStatement [t] = (Left $ parseError t "Expect variable name.", [t])
varDeclarationStatement _ = error "should at least have EOF in varDeclarationStatement 3"

statement :: [Token] -> ParseStatementResult
statement (t : rest)
  | ttype == EOF = (Left "", [])
  | ttype == PRINT = printStatement $ t : rest
  | ttype == LEFT_BRACE = blockStatement $ t : rest
  | otherwise = expressionStatement $ t : rest
  where
    ttype = tokenType t
statement [] = error "Should have at least EOF in statement"

blockStatement :: [Token] -> ParseStatementResult
blockStatement (_ : rest) =
  let (blockOrErr, leftovers) = buildBlock rest [] in
    case blockOrErr of
      Right block -> (Right $ Block $ reverse block, leftovers)
      Left err -> (Left err, leftovers)
  where
    buildBlock (t1 : tRest) buildup
      | ttype == RIGHT_BRACE = (Right buildup, tRest)
      | ttype == EOF = (Left $ parseError t1 "Expect '}' after block.", tRest)
      | otherwise = let ((stmtOrErr, toks), _) = declaration $ t1 : tRest in
          case stmtOrErr of
            Right stmt -> buildBlock toks $ stmt : buildup
            Left err -> (Left err, [last toks])
      where ttype = tokenType t1
    buildBlock [] _ = error "Should not have empty in blockStatement helper"
blockStatement _ = error "Should not have empty in blockStatement"

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
      else (Left $ parseError t "Expect ';' after value.", t : leftovers)
  (Left err, leftovers) -> (Left err, leftovers)
  _ -> error "Shouldn't have empty tokens in expressionStatement"

assignment :: [Token] -> ParseExpressionResult
assignment ts = case equality ts of
  (Left err, leftovers) -> (Left err, leftovers)
  (Right expr, t : leftovers) ->
    if tokenType t == EQUAL
      then case assignment leftovers of
        (Left err, afterValue) -> (Left err, afterValue)
        (Right value, afterValue) -> case expr of
          (Variable token) -> (Right $ Assign token value, afterValue)
          _ -> (Left $ parseError t "Invalid assignment target.", afterValue)
      else (Right expr, t : leftovers)
  _ -> error "Shouldn't have empty tokens in assignment"

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
  | tokenType token == IDENTIFIER =
      (Right $ Variable token, rest)
  | otherwise = (Left $ parseError token "Expect expression.", rest)
primary _ = error "should always at least EOF in primary"

synchronize :: [Token] -> [Token]
synchronize (t : toks)
  | ttype == EOF = t : toks
  | ttype == SEMICOLON = toks
  | ttype `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN] = t : toks
  | otherwise = synchronize toks
  where ttype = tokenType t
synchronize [] = error "should not get empty in sync"

addToResult :: ParseResult -> Either String Stmt -> ParseResult
addToResult (Right stmts) (Right stmt) = Right $ stmt : stmts
addToResult (Left errs) (Left err) = Left $ err : errs
addToResult (Right _) (Left err) = Left [err]
addToResult errs _ = errs
