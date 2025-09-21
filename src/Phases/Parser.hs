module Phases.Parser (parse, ParseResult, expression) where

import           Error
import           Phases.Expr
import           Phases.Stmt
import           Tokens

type ParseResult = Either [String] [Stmt]

type ParseExpressionResult = Either (String, [Token]) (Expr, [Token])

type ParseStatementResult = Either (String, [Token]) (Stmt, [Token])

parse :: [Token] -> ParseResult
parse input = case parseHelper input $ Right [] of
  Left errs   -> Left $ reverse errs
  Right stmts -> Right $ reverse stmts
  where
    parseHelper :: [Token] -> ParseResult -> ParseResult
    parseHelper tokens result =
      let (decRes, shouldContinue) = declaration tokens
          (parsedResult, leftovers) = case decRes of
            Right (f, s) -> (Right f, s)
            Left (f, s)  -> (Left f, s)
        in if shouldContinue
          then parseHelper leftovers $ addToResult result parsedResult
          else result

declaration :: [Token] -> (ParseStatementResult, Bool)
declaration (t : rest)
  | ttype == EOF = (Left ("", []), False)
  | ttype == VAR = case varDeclarationStatement $ t : rest of
      Right (expr, leftovers) -> (Right (expr, leftovers), True)
      Left (err, leftovers)   -> (Left (err, synchronize leftovers), True)
  | otherwise = case statement $ t : rest of
      Right (expr, leftovers) -> (Right (expr, leftovers), True)
      Left (err, leftovers)   -> (Left (err, synchronize leftovers), True)
  where
    ttype = tokenType t
declaration _ = error "should at least have EOF in declaration"

expression :: [Token] -> ParseExpressionResult
expression = assignment

statement :: [Token] -> ParseStatementResult
statement (t : rest)
  | ttype == EOF = Left ("", [])
  | ttype == PRINT = printStatement $ t : rest
  | ttype == IF = ifStatement $ t : rest
  | ttype == WHILE = whileStatement $ t : rest
  | ttype == FOR = forStatement $ t : rest
  | ttype == LEFT_BRACE = blockStatement $ t : rest
  | otherwise = expressionStatement $ t : rest
  where
    ttype = tokenType t
statement [] = error "Should have at least EOF in statement"

forStatement :: [Token] -> ParseStatementResult
forStatement (_ : afterFor) = do
  (_, afterLeftParen) <- consume LEFT_PAREN afterFor "Expect '(' after for."
  (initializer, afterInitializer) <- getInitializer afterLeftParen
  (condition, afterCondition) <- getCondition afterInitializer
  (_, afterSecondSemi) <- consume SEMICOLON afterCondition "Expect ';' after loop condition."
  (increment, afterIncrement) <- getIncrement afterSecondSemi
  (_, afterRightParen) <- consume RIGHT_PAREN afterIncrement "Expect ')' after for clauses."
  (body, afterBody) <- statement afterRightParen
  let desugared = desugarToWhile initializer condition increment body
  return (desugared, afterBody)
  where
    desugarToWhile :: Maybe Stmt -> Maybe Expr -> Maybe Expr -> Stmt -> Stmt
    desugarToWhile initializer condition increment body =
      let newBody = (case increment of
                       Nothing  -> body
                       Just inc -> Block [body, Expression inc])
          newCondition = (case condition of
                            Nothing   -> Primary $ Boolean True
                            Just cond -> cond)
          newStmt = let while = While newCondition newBody
                      in (case initializer of
                            Nothing      -> while
                            Just initial -> Block [initial, while])
        in newStmt
    getIncrement :: [Token] -> Either (String, [Token]) (Maybe Expr, [Token])
    getIncrement toks
      | (isMatch, _) <- match [RIGHT_PAREN] toks, isMatch = return (Nothing, toks)
      | otherwise = do
          (increment, afterIncrement) <- expression toks
          return (Just increment, afterIncrement)
    getCondition :: [Token] -> Either (String, [Token]) (Maybe Expr, [Token])
    getCondition toks
      | (isMatch, _) <- match [SEMICOLON] toks, isMatch = return (Nothing, toks)
      | otherwise = do
          (condition, afterCondition) <- expression toks
          return (Just condition, afterCondition)
    getInitializer :: [Token] -> Either (String, [Token]) (Maybe Stmt, [Token])
    getInitializer toks
      | (isMatch, afterSemi) <- match [SEMICOLON] toks, isMatch = return (Nothing, afterSemi)
      | (isMatch, _) <- match [VAR] toks, isMatch = do
          (initializer, afterInitializer) <- varDeclarationStatement toks
          return (Just initializer, afterInitializer)
      | otherwise = do
          (initializer, afterInitializer) <- expressionStatement toks
          return (Just initializer, afterInitializer)
forStatement [] = error "Should have at least for in forStatement"

whileStatement :: [Token] -> ParseStatementResult
whileStatement (_ : afterWhile) = do
  (_, afterOpenParen) <- consume LEFT_PAREN afterWhile "Expect '(' after while."
  (expr, afterExpr) <- expression afterOpenParen
  (_, afterCloseParen) <- consume RIGHT_PAREN afterExpr "Expect ')' after condition."
  (stmt, afterStmt) <- statement afterCloseParen
  return (While expr stmt, afterStmt)
whileStatement [] = error "Should have at least While in whileStatement"

varDeclarationStatement :: [Token] -> ParseStatementResult
varDeclarationStatement [t] = Left (parseError t "Expect variable name.", [t])
varDeclarationStatement (_ : rest) = do
  (t1, afterIdentifier) <- consume IDENTIFIER rest "Expect variable name."
  case afterIdentifier of
    t2 : afterEqual -> case tokenType t2 of
      EQUAL -> do
        (expr, afterExpr) <- expression afterEqual
        (_, leftovers) <- consume SEMICOLON afterExpr "Expect ';' after variable declaration."
        return (Var t1 expr, leftovers)
      SEMICOLON -> return (Var t1 (Primary Nil), afterEqual) -- TODO: make the value here Nothing
      _ ->
        Left
          ( parseError t2 "Expect ';' after variable declaration.",
            synchronize $ t2 : afterEqual
          )
    _ -> error "Should at least have EOF in varDeclarationStatement 2"
varDeclarationStatement _ = error "should at least have EOF in varDeclarationStatement 3"

ifStatement :: [Token] -> ParseStatementResult
ifStatement (_ : ts) = do
  (_, afterLeftParen) <- consume LEFT_PAREN ts "Expect '(' after 'if'."
  (expr, afterCondition) <- expression afterLeftParen
  (_, afterRightParen) <- consume RIGHT_PAREN afterCondition "Expect ')' after if condition."
  (ifBranch, afterIfBranch) <- statement afterRightParen
  case afterIfBranch of
    (elseKeyword' : afterElseKeyword) ->
      if tokenType elseKeyword' == ELSE
        then do
          (elseBranch, afterElseBranch) <- statement afterElseKeyword
          return (If expr ifBranch $ Just elseBranch, afterElseBranch)
        else return (If expr ifBranch Nothing, elseKeyword' : afterElseKeyword)
    [] -> error "Should have at least EOF in ifStatement after else keyword"
ifStatement [] = error "Should have at least EOF in ifStatement"

blockStatement :: [Token] -> ParseStatementResult
blockStatement (_ : rest) = do
  (blockStatements, leftovers) <- buildBlock rest []
  return (Block $ reverse blockStatements, leftovers)
  where
    buildBlock :: [Token] -> [Stmt] -> Either (String, [Token]) ([Stmt], [Token])
    buildBlock (t1 : tRest) buildup
      | ttype == RIGHT_BRACE = return (buildup, tRest)
      | ttype == EOF = Left (parseError t1 "Expect '}' after block.", tRest)
      | otherwise = do
          case declaration $ t1 : tRest of
            (Right (stmt, leftovers), _) -> buildBlock leftovers $ stmt : buildup
            (Left (err, leftovers), _) -> Left (err, leftovers)
      where
        ttype = tokenType t1
    buildBlock [] _ = error "Should not have empty in blockStatement helper"
blockStatement _ = error "Should not have empty in blockStatement"

printStatement :: [Token] -> ParseStatementResult
printStatement (_ : afterPrint) = do
  (expr, afterExpr) <- expression afterPrint
  (_, leftovers) <- consume SEMICOLON afterExpr "Expect ';' after expression."
  return (Print expr, leftovers)
printStatement [] = error "Shouln't have empty tokens in printStatement"

expressionStatement :: [Token] -> ParseStatementResult
expressionStatement ts = do
  (expr, afterExpr) <- expression ts
  (_, leftovers) <- consume SEMICOLON afterExpr "Expect ';' after expression."
  return (Expression expr, leftovers)

assignment :: [Token] -> ParseExpressionResult
assignment ts = do
  (expr, afterExpr) <- orExpr ts
  case afterExpr of
    (t : leftovers) ->
      if tokenType t == EQUAL
        then do
          (value, afterValue) <- assignment leftovers
          case expr of
            (Variable token) -> return (Assign token value, afterValue)
            _ -> Left (parseError t "Invalid assignment target.", afterValue)
        else return (expr, afterExpr)
    [] -> error "Shouldn't have empty tokens in assignment"

orExpr :: [Token] -> ParseExpressionResult
orExpr = chainedOperator andExpr [OR] OrExpr

andExpr :: [Token] -> ParseExpressionResult
andExpr = chainedOperator equality [AND] AndExpr

equality :: [Token] -> ParseExpressionResult
equality = chainedOperator comparison [EQUAL_EQUAL, BANG_EQUAL] Binary

comparison :: [Token] -> ParseExpressionResult
comparison = chainedOperator term [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL] Binary

term :: [Token] -> ParseExpressionResult
term = chainedOperator factor [PLUS, MINUS] Binary

factor :: [Token] -> ParseExpressionResult
factor = chainedOperator unary [SLASH, STAR] Binary

chainedOperator ::
  ([Token] -> ParseExpressionResult) ->
  [TokenType] ->
  (Expr -> Token -> Expr -> Expr) ->
  ([Token] -> ParseExpressionResult)
chainedOperator innerParseF matchTokenTypes exprConstructor tokens = do
  (left, afterLeft) <- innerParseF tokens
  case afterLeft of
    (op : rest) ->
      if tokenType op `elem` matchTokenTypes
        then chainedOperatorHelper left op rest
        else return (left, afterLeft)
    [] -> error $ "should at least see EOF in helper for " ++ show matchTokenTypes
  where
    chainedOperatorHelper :: Expr -> Token -> [Token] -> ParseExpressionResult
    chainedOperatorHelper left op afterOp = do
      (right, afterRight) <- innerParseF afterOp
      case afterRight of
        (afterRightOp : rest) ->
          if tokenType afterRightOp `elem` matchTokenTypes
            then chainedOperatorHelper (exprConstructor left op right) afterRightOp rest
            else return (exprConstructor left op right, afterRight)
        [] -> error $ "should at least see EOF in helper for helper for " ++ show matchTokenTypes

unary :: [Token] -> ParseExpressionResult
unary (token : rest)
  | tokenType token `elem` [MINUS, BANG] = do
      (right, leftovers) <- unary rest
      return (Unary token right, leftovers)
  | otherwise = primary (token : rest)
unary [] = error "should at least see EOF in unary"

primary :: [Token] -> ParseExpressionResult
primary (token : rest)
  | tokenType token `elem` [FALSE, TRUE, NUMBER, STRING, NIL] =
      return (Primary $ literal token, rest)
  | tokenType token == LEFT_PAREN = do
      (inner, afterExpr) <- expression rest
      (_, afterRightParen) <- consume RIGHT_PAREN afterExpr "Expect ')' after expression."
      return (Grouping inner, afterRightParen)
  | tokenType token == IDENTIFIER = return (Variable token, rest)
  | otherwise = Left (parseError token "Expect expression.", rest)
primary _ = error "should always at least EOF in primary"

match :: [TokenType] -> [Token] -> (Bool, [Token])
match types (t : toks) = let isMatch = tokenType t `elem` types
  in (isMatch, if isMatch then toks else t : toks)
match _     []         = error "Should have at least EOF in match"

consume :: TokenType -> [Token] -> String -> Either (String, [Token]) (Token, [Token])
consume ttype (t1 : rest) errMsg = if tokenType t1 == ttype
  then Right (t1, rest)
  else Left (parseError t1 errMsg, rest)
consume _     []          _      = error "Should have at least EOF in consume"

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
addToResult (Left errs) (Left err)     = Left $ err : errs
addToResult (Right _) (Left err)       = Left [err]
addToResult errs _                     = errs
