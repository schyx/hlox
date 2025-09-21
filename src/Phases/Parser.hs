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
declaration tokens
  | (Right _, _) <- matchFirst [EOF] tokens = (Left ("", []), False)
  | (Right _, _) <- matchFirst [VAR] tokens = case varDeclarationStatement tokens of
      Right (expr, leftovers) -> (Right (expr, leftovers), True)
      Left (err, leftovers)   -> (Left (err, synchronize leftovers), True)
  | otherwise = case statement tokens of
      Right (expr, leftovers) -> (Right (expr, leftovers), True)
      Left (err, leftovers)   -> (Left (err, synchronize leftovers), True)

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
      | (Right _, _) <- matchFirst [RIGHT_PAREN] toks = return (Nothing, toks)
      | otherwise = do
          (increment, afterIncrement) <- expression toks
          return (Just increment, afterIncrement)
    getCondition :: [Token] -> Either (String, [Token]) (Maybe Expr, [Token])
    getCondition toks
      | (Right _, _) <- matchFirst [SEMICOLON] toks = return (Nothing, toks)
      | otherwise = do
          (condition, afterCondition) <- expression toks
          return (Just condition, afterCondition)
    getInitializer :: [Token] -> Either (String, [Token]) (Maybe Stmt, [Token])
    getInitializer toks
      | (Right _, afterSemi) <- matchFirst [SEMICOLON] toks = return (Nothing, afterSemi)
      | (Right _, _) <- matchFirst [VAR] toks = do
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
  (elseBranch, afterElseBranch) <- getElseBranch afterIfBranch
  return (If expr ifBranch elseBranch, afterElseBranch)
  where
    getElseBranch :: [Token] -> Either (String, [Token]) (Maybe Stmt, [Token])
    getElseBranch toks
      | (Right _, afterElseKeyword) <- matchFirst [ELSE] toks = do
          (elseStmt, afterElseBranch) <- statement afterElseKeyword
          return (Just elseStmt, afterElseBranch)
      | otherwise = return (Nothing, toks)
ifStatement [] = error "Should have at least EOF in ifStatement"

blockStatement :: [Token] -> ParseStatementResult
blockStatement (_ : rest) = do
  (blockStatements, leftovers) <- buildBlock rest []
  return (Block $ reverse blockStatements, leftovers)
  where
    buildBlock :: [Token] -> [Stmt] -> Either (String, [Token]) ([Stmt], [Token])
    buildBlock toks buildup
      | (Right _, tRest) <- matchFirst [RIGHT_BRACE] toks = return (buildup, tRest)
      | (Right t, tRest) <- matchFirst [EOF] toks = Left (parseError t "Expect '}' after block.", tRest)
      | otherwise = do
          case declaration toks of
            (Right (stmt, leftovers), _) -> buildBlock leftovers $ stmt : buildup
            (Left (err, leftovers), _) -> Left (err, leftovers)
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
  let (isMatch, afterRHS) = matchFirst [EQUAL] afterExpr
  case isMatch of
    Right t -> do
      (value, afterValue) <- assignment afterRHS
      case expr of
        (Variable token) -> return (Assign token value, afterValue)
        _ -> Left (parseError t "Invalid assignment target.", afterValue)
    Left _ -> return (expr, afterExpr)

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
  case matchFirst matchTokenTypes afterLeft of
    (Right op, afterOp) -> chainedOperatorHelper left op afterOp
    (Left (), rest)     -> return (left, rest)
  where
    chainedOperatorHelper :: Expr -> Token -> [Token] -> ParseExpressionResult
    chainedOperatorHelper left op afterOp = do
      (right, afterRight) <- innerParseF afterOp
      case matchFirst matchTokenTypes afterRight of
        (Right afterRightOp, rest) ->
          chainedOperatorHelper (exprConstructor left op right) afterRightOp rest
        (Left (), rest) -> return (exprConstructor left op right, rest)

unary :: [Token] -> ParseExpressionResult
unary toks = case matchFirst [MINUS, BANG] toks of
  (Right token, afterOp) -> do
    (right, leftovers) <- unary afterOp
    return (Unary token right, leftovers)
  (Left (), afterCheck) -> primary afterCheck

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

matchFirst :: [TokenType] -> [Token] -> (Either () Token, [Token])
matchFirst types (t : toks) = let isMatch = tokenType t `elem` types
  in (if isMatch then Right t else Left (), if isMatch then toks else t : toks)
matchFirst _     []         = error "Should have at least EOF in match"

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
