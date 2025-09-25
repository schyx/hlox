module Phases.Parser (parse, ParseResult, expression) where

import           Error
import           Phases.Expr
import           Phases.Stmt
import           Tokens

type ParseResult = Either [String] [Stmt]

type ParseExpressionResult = Either ([String], [Token]) (Expr, [Token], [String])

type ParseStatementResult = Either ([String], [Token]) (Stmt, [Token], [String])

parse :: [Token] -> ParseResult
parse input = case parseHelper input $ Right [] of
  Left errs   -> Left $ reverse errs
  Right stmts -> Right $ reverse stmts
  where
    parseHelper :: [Token] -> ParseResult -> ParseResult
    parseHelper tokens result =
      let (decRes, shouldContinue) = declaration tokens []
          (parsedResult, leftovers) = case decRes of
            Right (f, s, e) ->
              case e of
                [] -> (Right f, s)
                _  -> (Left e, s)
            Left (f, s)  -> (Left f, s)
        in if shouldContinue
          then parseHelper leftovers $ addToResult result parsedResult
          else result

declaration :: [Token] -> [String] -> (ParseStatementResult, Bool)
declaration tokens inErrs
  | (Right _, _) <- matchFirst [EOF] tokens = (Left ([""], []), False)
  | (Right _, _) <- matchFirst [VAR] tokens = case varDeclarationStatement tokens inErrs of
      Right (expr, leftovers, errs) -> (Right (expr, leftovers, errs ++ inErrs), True)
      Left (err, leftovers)   -> (Left (err, synchronize leftovers), True)
  | otherwise = case statement tokens inErrs of
      Right (expr, leftovers, errs) -> (Right (expr, leftovers, errs ++ inErrs), True)
      Left (err, leftovers)   -> (Left (err, synchronize leftovers), True)

expression :: [Token] -> [String] -> ParseExpressionResult
expression = assignment

statement :: [Token] -> [String] -> ParseStatementResult
statement (t : rest) inErrs
  | ttype == EOF = Left ([""], [])
  | ttype == PRINT = printStatement (t : rest) inErrs
  | ttype == IF = ifStatement (t : rest) inErrs
  | ttype == WHILE = whileStatement (t : rest) inErrs
  | ttype == FOR = forStatement (t : rest) inErrs
  | ttype == LEFT_BRACE = blockStatement (t : rest) inErrs
  | otherwise = expressionStatement (t : rest) inErrs
  where
    ttype = tokenType t
statement [] _ = error "Should have at least EOF in statement"

forStatement :: [Token] -> [String] -> ParseStatementResult
forStatement (_ : afterFor) inErrs = do
  (_, afterLeftParen, afterLeftParenErrs) <- consume LEFT_PAREN afterFor "Expect '(' after for." inErrs
  (initializer, afterInitializer, afterInitializerErrs) <- getInitializer afterLeftParen afterLeftParenErrs
  (condition, afterCondition, afterConditionErrs) <- getCondition afterInitializer afterInitializerErrs
  (_, afterSecondSemi, afterSecondSemiErrs) <- consume SEMICOLON afterCondition "Expect ';' after loop condition." afterConditionErrs
  (increment, afterIncrement, afterIncrementErrs) <- getIncrement afterSecondSemi afterSecondSemiErrs
  (_, afterRightParen, afterRightParenErrs) <- consume RIGHT_PAREN afterIncrement "Expect ')' after for clauses." afterIncrementErrs
  (body, afterBody, afterBodyErrs) <- statement afterRightParen afterRightParenErrs
  let desugared = desugarToWhile initializer condition increment body
  return (desugared, afterBody, afterBodyErrs)
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
    getIncrement :: [Token] -> [String] -> Either ([String], [Token]) (Maybe Expr, [Token], [String])
    getIncrement toks inIncrementErrs
      | (Right _, _) <- matchFirst [RIGHT_PAREN] toks = return (Nothing, toks, inIncrementErrs)
      | otherwise = do
          (increment, afterIncrement, afterIncrementErrs) <- expression toks inIncrementErrs
          return (Just increment, afterIncrement, afterIncrementErrs)
    getCondition :: [Token] -> [String] -> Either ([String], [Token]) (Maybe Expr, [Token], [String])
    getCondition toks inConditionErrs
      | (Right _, _) <- matchFirst [SEMICOLON] toks = return (Nothing, toks, inConditionErrs)
      | otherwise = do
          (condition, afterCondition, afterConditionErrs) <- expression toks inConditionErrs
          return (Just condition, afterCondition, afterConditionErrs)
    getInitializer :: [Token] -> [String] -> Either ([String], [Token]) (Maybe Stmt, [Token], [String])
    getInitializer toks inInitializerErrs
      | (Right _, afterSemi) <- matchFirst [SEMICOLON] toks = return (Nothing, afterSemi, inInitializerErrs)
      | (Right _, _) <- matchFirst [VAR] toks = do
          (initializer, afterInitializer, afterInitializerErrs) <- varDeclarationStatement toks inInitializerErrs
          return (Just initializer, afterInitializer, afterInitializerErrs)
      | otherwise = do
          (initializer, afterInitializer, afterInitializerErrs) <- expressionStatement toks inInitializerErrs
          return (Just initializer, afterInitializer, afterInitializerErrs)
forStatement [] _ = error "Should have at least for in forStatement"

whileStatement :: [Token] -> [String] -> ParseStatementResult
whileStatement (_ : afterWhile) inErrs = do
  (_, afterOpenParen, openParenErrs) <- consume LEFT_PAREN afterWhile "Expect '(' after while." inErrs
  (expr, afterExpr, exprErrs) <- expression afterOpenParen openParenErrs
  (_, afterCloseParen, closeParenErrs) <- consume RIGHT_PAREN afterExpr "Expect ')' after condition." exprErrs
  (stmt, afterStmt, stmtErrs) <- statement afterCloseParen closeParenErrs
  return (While expr stmt, afterStmt, stmtErrs)
whileStatement [] _ = error "Should have at least While in whileStatement"

varDeclarationStatement :: [Token] -> [String] -> ParseStatementResult
varDeclarationStatement [t] inErrs = Left (parseError t "Expect variable name." : inErrs, [t])
varDeclarationStatement (_ : rest) inErrs = do
  (t1, afterIdentifier, identifierErrs) <- consume IDENTIFIER rest "Expect variable name." inErrs
  case afterIdentifier of
    t2 : afterEqual -> case tokenType t2 of
      EQUAL -> do
        (expr, afterExpr, exprErrs) <- expression afterEqual identifierErrs
        (_, leftovers, semiErrs) <- consume SEMICOLON afterExpr "Expect ';' after variable declaration." exprErrs
        return (Var t1 expr, leftovers, semiErrs)
      SEMICOLON -> return (Var t1 (Primary Nil), afterEqual, identifierErrs) -- TODO: make the value here Nothing
      _ ->
        Left
          ( parseError t2 "Expect ';' after variable declaration." : identifierErrs,
            synchronize $ t2 : afterEqual
          )
    _ -> error "Should at least have EOF in varDeclarationStatement 2"
varDeclarationStatement _ _ = error "should at least have EOF in varDeclarationStatement 3"

ifStatement :: [Token] -> [String] -> ParseStatementResult
ifStatement (_ : ts) inErrs = do
  (_, afterLeftParen, afterLeftParenErrs) <- consume LEFT_PAREN ts "Expect '(' after 'if'." inErrs
  (expr, afterCondition, conditionErrs) <- expression afterLeftParen afterLeftParenErrs
  (_, afterRightParen, rightParenErrs) <- consume RIGHT_PAREN afterCondition "Expect ')' after if condition." conditionErrs
  (ifBranch, afterIfBranch, ifBranchErrs) <- statement afterRightParen rightParenErrs
  (elseBranch, afterElseBranch, elseBranchErrs) <- getElseBranch afterIfBranch ifBranchErrs
  return (If expr ifBranch elseBranch, afterElseBranch, elseBranchErrs)
  where
    getElseBranch :: [Token] -> [String] -> Either ([String], [Token]) (Maybe Stmt, [Token], [String])
    getElseBranch toks elseErrs
      | (Right _, afterElseKeyword) <- matchFirst [ELSE] toks = do
          (elseStmt, afterElseBranch, afterElseErrs) <- statement afterElseKeyword elseErrs
          return (Just elseStmt, afterElseBranch, afterElseErrs)
      | otherwise = return (Nothing, toks, elseErrs)
ifStatement [] _ = error "Should have at least EOF in ifStatement"

blockStatement :: [Token] -> [String] -> ParseStatementResult
blockStatement (_ : rest) inErrs = do
  (blockStatements, leftovers, blockErrs) <- buildBlock rest [] inErrs
  return (Block $ reverse blockStatements, leftovers, blockErrs)
  where
    buildBlock :: [Token] -> [Stmt] -> [String] -> Either ([String], [Token]) ([Stmt], [Token], [String])
    buildBlock toks buildup inBlockErrs
      | (Right _, tRest) <- matchFirst [RIGHT_BRACE] toks = return (buildup, tRest, inBlockErrs)
      | (Right t, tRest) <- matchFirst [EOF] toks = Left (parseError t "Expect '}' after block." : inBlockErrs, tRest)
      | otherwise = do
          case declaration toks inBlockErrs of
            (Right (stmt, leftovers, stmtErrs), _) -> buildBlock leftovers (stmt : buildup) stmtErrs
            (Left (err, leftovers), _) -> Left (err, leftovers)
blockStatement _ _ = error "Should not have empty in blockStatement"

printStatement :: [Token] -> [String] -> ParseStatementResult
printStatement (_ : afterPrint) inErrs = do
  (expr, afterExpr, afterExprErrs) <- expression afterPrint inErrs
  (_, leftovers, afterSemiErrs) <- consume SEMICOLON afterExpr "Expect ';' after expression." afterExprErrs
  return (Print expr, leftovers, afterSemiErrs)
printStatement [] _ = error "Shouln't have empty tokens in printStatement"

expressionStatement :: [Token] -> [String] -> ParseStatementResult
expressionStatement ts inErrs = do
  (expr, afterExpr, afterExprErrs) <- expression ts inErrs
  (_, leftovers, afterConsumeErrs) <- consume SEMICOLON afterExpr "Expect ';' after expression." afterExprErrs
  return (Expression expr, leftovers, afterConsumeErrs)

assignment :: [Token] -> [String] -> ParseExpressionResult
assignment ts inErrs = do
  (expr, afterExpr, exprErrs) <- orExpr ts inErrs
  let (isMatch, afterRHS) = matchFirst [EQUAL] afterExpr
  case isMatch of
    Right t -> do
      (value, afterValue, valueErrs) <- assignment afterRHS exprErrs
      case expr of
        (Variable token) -> return (Assign token value, afterValue, valueErrs)
        _ -> Left (parseError t "Invalid assignment target." : valueErrs, afterValue)
    Left _ -> return (expr, afterExpr, exprErrs)

orExpr :: [Token] -> [String] -> ParseExpressionResult
orExpr = chainedOperator andExpr [OR] OrExpr

andExpr :: [Token] -> [String] -> ParseExpressionResult
andExpr = chainedOperator equality [AND] AndExpr

equality :: [Token] -> [String] -> ParseExpressionResult
equality = chainedOperator comparison [EQUAL_EQUAL, BANG_EQUAL] Binary

comparison :: [Token] -> [String] -> ParseExpressionResult
comparison = chainedOperator term [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL] Binary

term :: [Token] -> [String] -> ParseExpressionResult
term = chainedOperator factor [PLUS, MINUS] Binary

factor :: [Token] -> [String] -> ParseExpressionResult
factor = chainedOperator unary [SLASH, STAR] Binary

chainedOperator ::
  ([Token] -> [String] -> ParseExpressionResult) ->
  [TokenType] ->
  (Expr -> Token -> Expr -> Expr) ->
  ([Token] -> [String] -> ParseExpressionResult)
chainedOperator innerParseF matchTokenTypes exprConstructor tokens inErrs = do
  (left, afterLeft, leftErrs) <- innerParseF tokens inErrs
  case matchFirst matchTokenTypes afterLeft of
    (Right op, afterOp) -> chainedOperatorHelper left op afterOp leftErrs
    (Left (), rest)     -> return (left, rest, leftErrs)
  where
    chainedOperatorHelper :: Expr -> Token -> [Token] -> [String] -> ParseExpressionResult
    chainedOperatorHelper left op afterOp inHelperErrs = do
      (right, afterRight, rightErrs) <- innerParseF afterOp inHelperErrs
      case matchFirst matchTokenTypes afterRight of
        (Right afterRightOp, rest) ->
          chainedOperatorHelper (exprConstructor left op right) afterRightOp rest rightErrs
        (Left (), rest) -> return (exprConstructor left op right, rest, rightErrs)

unary :: [Token] -> [String] -> ParseExpressionResult
unary toks inErrs = case matchFirst [MINUS, BANG] toks of
  (Right token, afterOp) -> do
    (right, leftovers, rightErrs) <- unary afterOp inErrs
    return (Unary token right, leftovers, rightErrs)
  (Left (), afterCheck) -> call afterCheck inErrs

call :: [Token] -> [String] -> ParseExpressionResult
call tokens inErrs = do
  (callee, afterCallee, calleeErrs) <- primary tokens inErrs
  let (isMatch, afterMatch) = matchFirst [LEFT_PAREN] afterCallee
  case isMatch of
    Left ()         -> return (callee, afterMatch, calleeErrs)
    Right leftParen -> do
      (leftCall, afterLeftCall, leftCallErrs) <- finishCall callee leftParen afterMatch calleeErrs
      callHelper leftCall afterLeftCall leftCallErrs
  where
    callHelper :: Expr -> [Token] -> [String] -> ParseExpressionResult
    callHelper leftCall afterLeftCall helperErrs =
      case matchFirst [LEFT_PAREN] afterLeftCall of
        (Left (), _)                      -> return (leftCall, afterLeftCall, helperErrs)
        (Right leftParen, afterLeftParen) -> do
          (newCall, afterNewCall, newCallErrs) <- finishCall leftCall leftParen afterLeftParen helperErrs
          callHelper newCall afterNewCall newCallErrs

finishCall :: Expr -> Token -> [Token] -> [String] -> ParseExpressionResult
finishCall callee leftParen argStart inErrs = case matchFirst [RIGHT_PAREN] argStart of
  (Right _, afterRightParen) -> return (Call callee leftParen [], afterRightParen, inErrs)
  (Left (), _) -> do
    (args, afterArgs, argErrs) <- go argStart [] inErrs
    return (Call callee leftParen $ reverse args, afterArgs, argErrs)
  where
    go :: [Token] -> [Expr] -> [String] -> Either ([String], [Token]) ([Expr], [Token], [String])
    go toks buildup goErrs = do
      let argNumErr = if length buildup >= 255
                        then parseError (head toks) "Can't have more than 255 elements" : goErrs
                        else goErrs
      (expr, afterExpr, exprErrs) <- expression toks argNumErr
      let newBuildup = expr : buildup
      case matchFirst [COMMA] afterExpr of
        (Left (), _) -> do
          (_, afterRightParen, rightParenErrs) <- consume RIGHT_PAREN afterExpr "Expect ')' after arguments." exprErrs
          return (newBuildup, afterRightParen, rightParenErrs)
        (Right _, afterComma) -> go afterComma newBuildup exprErrs

primary :: [Token] -> [String] -> ParseExpressionResult
primary (token : rest) inErrs
  | tokenType token `elem` [FALSE, TRUE, NUMBER, STRING, NIL] =
      return (Primary $ literal token, rest, inErrs)
  | tokenType token == LEFT_PAREN = do
      (inner, afterExpr, innerErrs) <- expression rest inErrs
      (_, afterRightParen, rightParenErrs) <- consume RIGHT_PAREN afterExpr "Expect ')' after expression." innerErrs
      return (Grouping inner, afterRightParen, rightParenErrs)
  | tokenType token == IDENTIFIER = return (Variable token, rest, inErrs)
  | otherwise = Left (parseError token "Expect expression." : inErrs, rest)
primary _ _ = error "should always at least EOF in primary"

matchFirst :: [TokenType] -> [Token] -> (Either () Token, [Token])
matchFirst types (t : toks) = let isMatch = tokenType t `elem` types
  in (if isMatch then Right t else Left (), if isMatch then toks else t : toks)
matchFirst _     []         = error "Should have at least EOF in match"

consume :: TokenType -> [Token] -> String -> [String] -> Either ([String], [Token]) (Token, [Token], [String])
consume ttype (t1 : rest) errMsg inErrs = if tokenType t1 == ttype
  then Right (t1, rest, inErrs)
  else Left (parseError t1 errMsg : inErrs, rest)
consume _     []          _      _      = error "Should have at least EOF in consume"

synchronize :: [Token] -> [Token]
synchronize (t : toks)
  | ttype == EOF = t : toks
  | ttype == SEMICOLON = toks
  | ttype `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN] = t : toks
  | otherwise = synchronize toks
  where ttype = tokenType t
synchronize [] = error "should not get empty in sync"

addToResult :: ParseResult -> Either [String] Stmt -> ParseResult
addToResult (Right stmts) (Right stmt)    = Right $ stmt : stmts
addToResult (Left oldErrs) (Left newErrs) = Left $ newErrs ++ oldErrs
addToResult (Right _) (Left errs)         = Left errs
addToResult errs _                        = errs
