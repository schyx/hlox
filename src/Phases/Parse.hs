{-# LANGUAGE TupleSections #-}

module Phases.Parse (parse, TreeResult, expressionWrapper) where

import Control.Applicative (Alternative (many), (<|>))
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (fromMaybe, isNothing)
import Error (parseError)
import Parser (Parser (Parser, runParser))
import Phases.Expr
import Phases.Stmt
import Tokens

type TreeResult = Either [String] [Stmt]

type Planter = Parser ([String], [Token])

makePlanter :: (([String], [Token]) -> Maybe (([String], [Token]), a)) -> Planter a
makePlanter = Parser

runPlanter :: Planter a -> (([String], [Token]) -> Maybe (([String], [Token]), a))
runPlanter = runParser

parse :: [Token] -> TreeResult
parse input =
  let start = ([], input)
   in ( \val ->
          let value = fromMaybe undefined val
              errs = fst . fst $ value
              stmts = fromMaybe undefined $ sequenceA $ snd value
           in if null errs
                then Right stmts
                else Left errs
      )
        $ runPlanter (many planter) start

planter :: Planter (Maybe Stmt)
planter = do
  maybeStmt <- runMaybeT declaration
  when (isNothing maybeStmt) synchronize
  return maybeStmt

synchronize :: Planter ()
synchronize = makePlanter $ \(inErrs, toks) ->
  let syncTokens (t : tokens)
        | tokenType t == EOF = t : tokens
        | tokenType t == SEMICOLON = tokens
        | tokenType t `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN] = t : tokens
        | otherwise = syncTokens tokens
      syncTokens [] = error "Should not get empty in sync"
   in Just ((inErrs, syncTokens toks), ())

-- | Allows for choice on the Planter level rather than the MaybeT level
(<||>) :: MaybeT Planter a -> MaybeT Planter a -> MaybeT Planter a
MaybeT p1 <||> MaybeT p2 = MaybeT $ p1 <|> p2

infixl 3 <||>

-- | Checks that the first token satisfies the predicate
check :: (TokenType -> Bool) -> MaybeT Planter Token
check predicate = MaybeT $ makePlanter $ \(inErrs, toks) ->
  let t1 = head toks
   in if predicate $ tokenType t1
        then Just ((inErrs, toks), Just t1)
        else Nothing

getFirst :: MaybeT Planter Token
getFirst = check $ const True

match :: (TokenType -> Bool) -> MaybeT Planter Token
match predicate = MaybeT $ makePlanter $ \(inErrs, toks) ->
  let t1 = head toks
      rest = tail toks
   in if predicate $ tokenType t1
        then Just ((inErrs, rest), Just t1)
        else Nothing

consume :: TokenType -> String -> MaybeT Planter Token
consume desiredType msg = MaybeT $ makePlanter $ \(inErrs, toks) ->
  let t1 = head toks
      rest = tail toks
      outIfErr = parseError t1 msg : inErrs
   in Just $
        if tokenType t1 == desiredType
          then ((inErrs, rest), Just t1)
          else ((outIfErr, toks), Nothing)

addParseError :: String -> MaybeT Planter a
addParseError errMsg = MaybeT $ makePlanter $ \(inErrs, toks) ->
  Just ((errMsg : inErrs, toks), Nothing)

addNonBlockingParseError :: String -> MaybeT Planter ()
addNonBlockingParseError errMsg = MaybeT $ makePlanter $ \(inErrs, toks) ->
  Just ((errMsg : inErrs, toks), Just ())

expressionWrapper :: [Token] -> Either [String] Expr
expressionWrapper =
  (\((errs, _), expr) -> if null errs then Right expr else Left errs)
    . fromMaybe undefined
    . runPlanter (fromMaybe undefined <$> runMaybeT expression)
    . ([],)

declaration :: MaybeT Planter Stmt
declaration =
  varDeclaration
    <||> functionDeclaration
    <||> statement

functionDeclaration :: MaybeT Planter Stmt
functionDeclaration = do
  _ <- match (== FUN)
  funcName <- consume IDENTIFIER "Expect function name."
  _ <- consume LEFT_PAREN "Expect '(' after function name."
  params <- getParams
  _ <- consume LEFT_BRACE "Expect '{' before function body."
  body <- buildBlock []
  return $ Function funcName params body
 where
  getParams = addParam [] <||> endParamList []
  addParam buildup = do
    when (length buildup >= 255) $ do
      first <- getFirst
      let tooManyParamsMsg = parseError first "Can't have more than 255 parameters."
      addNonBlockingParseError tooManyParamsMsg
    param <- match (== IDENTIFIER)
    let newBuildup = param : buildup
    (match (== COMMA) >> addParam newBuildup)
      <||> endParamList newBuildup
  endParamList buildup = do
    _ <- consume RIGHT_PAREN "Expect ')' after parameters."
    return $ reverse buildup

varDeclaration :: MaybeT Planter Stmt
varDeclaration = do
  _ <- match (== VAR)
  varName <- consume IDENTIFIER "Expect variable name."
  assignVal varName
    <||> noVal varName
    <||> unexpectedToken
 where
  assignVal varName = do
    _ <- match (== EQUAL)
    expr <- expression
    _ <- consume SEMICOLON "Expect ';' after variable declaration."
    return $ Var varName expr
  noVal varName = do
    _ <- match (== SEMICOLON)
    return $ Var varName $ Primary Nil
  unexpectedToken =
    ( parseError
        <$> match (const True)
        <*> pure "Expect ';' after variable declaration."
    )
      >>= addParseError

statement :: MaybeT Planter Stmt
statement =
  forStatement
    <||> ifStatement
    <||> printStatement
    <||> returnStatement
    <||> whileStatement
    <||> blockStatement
    <||> expressionStatement

forStatement :: MaybeT Planter Stmt
forStatement = do
  _ <- match (== FOR)
  _ <- consume LEFT_PAREN "Expect '(' after for."
  initializer <-
    (match (== SEMICOLON) >> pure Nothing)
      <||> (Just <$> varDeclaration)
      <||> (Just <$> expressionStatement)
  condition <-
    (check (== SEMICOLON) >> pure Nothing)
      <||> (Just <$> expression)
  _ <- consume SEMICOLON "Expect ';' after loop condition."
  increment <-
    (check (== RIGHT_PAREN) >> pure Nothing)
      <||> (Just <$> expression)
  _ <- consume RIGHT_PAREN "Expect ')' after for clauses."
  desugarToWhile initializer condition increment <$> statement
 where
  desugarToWhile initializer condition increment body =
    let newBody = case increment of
          Nothing -> body
          Just inc -> Block [body, Expression inc]
        newCondition = case condition of
          Nothing -> Primary $ Boolean True
          Just cond -> cond
        newStmt =
          let while = While newCondition newBody
           in case initializer of
                Nothing -> while
                Just initial -> Block [initial, while]
     in newStmt

ifStatement :: MaybeT Planter Stmt
ifStatement = do
  _ <- match (== IF)
  _ <- consume LEFT_PAREN "Expect '(' after 'if'."
  expr <- expression
  _ <- consume RIGHT_PAREN "Expect ')' after if condition."
  ifBranch <- statement
  If expr ifBranch <$> getElseBranch
 where
  getElseBranch = (match (== ELSE) >> (Just <$> statement)) <||> pure Nothing

printStatement :: MaybeT Planter Stmt
printStatement = do
  _ <- match (== PRINT)
  expr <- expression
  _ <- consume SEMICOLON "Expect ';' after expression."
  return $ Print expr

returnStatement :: MaybeT Planter Stmt
returnStatement = do
  returnToken <- match (== RETURN)
  emptyReturn returnToken <||> hasExprReturn returnToken
 where
  emptyReturn returnToken = do
    _ <- match (== SEMICOLON)
    return (Return returnToken $ Primary Nil)
  hasExprReturn returnToken = do
    expr <- expression
    _ <- consume SEMICOLON "Expect ';' after return value."
    return $ Return returnToken expr

whileStatement :: MaybeT Planter Stmt
whileStatement = do
  _ <- match (== WHILE)
  _ <- consume LEFT_PAREN "Expect '(' after while."
  expr <- expression
  _ <- consume RIGHT_PAREN "Expect ')' after condition."
  While expr <$> statement

blockStatement :: MaybeT Planter Stmt
blockStatement = do
  _ <- match (== LEFT_BRACE)
  Block <$> buildBlock []

buildBlock :: [Stmt] -> MaybeT Planter [Stmt]
buildBlock buildup = rightBrace <||> endOfFile <||> moreStmts
 where
  rightBrace = match (== RIGHT_BRACE) >> return (reverse buildup)
  endOfFile = do
    eofToken <- check (== EOF)
    addParseError $ parseError eofToken "Expect '}' after block."
  moreStmts = do
    stmt <- declaration
    buildBlock $ stmt : buildup

expressionStatement :: MaybeT Planter Stmt
expressionStatement = do
  _ <- check (/= EOF)
  expr <- expression
  _ <- consume SEMICOLON "Expect ';' after expression."
  return $ Expression expr

expression :: MaybeT Planter Expr
expression = assignment

assignment :: MaybeT Planter Expr
assignment = do
  expr <- orExpr
  ( do
      equalSign <- match (== EQUAL)
      value <- assignment
      case expr of
        Variable token -> return $ Assign token value
        _ -> addParseError $ parseError equalSign "Invalid assignment target."
    )
    <||> return expr

orExpr :: MaybeT Planter Expr
orExpr = chainedOperator andExpr [OR] OrExpr

andExpr :: MaybeT Planter Expr
andExpr = chainedOperator equality [AND] AndExpr

equality :: MaybeT Planter Expr
equality = chainedOperator comparison [EQUAL_EQUAL, BANG_EQUAL] Binary

comparison :: MaybeT Planter Expr
comparison = chainedOperator term [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL] Binary

term :: MaybeT Planter Expr
term = chainedOperator factor [PLUS, MINUS] Binary

factor :: MaybeT Planter Expr
factor = chainedOperator unary [SLASH, STAR] Binary

chainedOperator ::
  MaybeT Planter Expr ->
  [TokenType] ->
  (Expr -> Token -> Expr -> Expr) ->
  MaybeT Planter Expr
chainedOperator innerPlanter opTypes exprConstructor = do
  left <- innerPlanter
  ( do
      op <- match (`elem` opTypes)
      chainedOperatorHelper left op
    )
    <||> return left
 where
  chainedOperatorHelper left op = do
    right <- innerPlanter
    let expr = exprConstructor left op right
    ( do
        op' <- match (`elem` opTypes)
        chainedOperatorHelper expr op'
      )
      <||> return expr

unary :: MaybeT Planter Expr
unary = (Unary <$> match (`elem` [MINUS, BANG]) <*> unary) <||> call

call :: MaybeT Planter Expr
call = callExpr <||> primary
 where -- TODO: refactor this maybe?
  callExpr = do
    callee <- primary
    leftParen <- match (== LEFT_PAREN)
    leftCall <- Call callee leftParen <$> finishCall
    callHelper leftCall
  callHelper leftCall =
    ( do
        newLeftParen <- match (== LEFT_PAREN)
        newCall <- Call leftCall newLeftParen <$> finishCall
        callHelper newCall
    )
      <||> pure leftCall
  finishCall = getArgs <||> (consume RIGHT_PAREN "Expect ')' after arguments." >> pure [])
  getArgs = do
    _ <- check (/= RIGHT_PAREN)
    argsHelper []
  argsHelper buildup =
    do
      when (length buildup >= 255) $ do
        first <- getFirst
        let tooManyArgsMsg = parseError first "Can't have more than 255 arguments."
        addNonBlockingParseError tooManyArgsMsg
      arg <- expression
      let newBuildup = arg : buildup
      (match (== COMMA) >> argsHelper newBuildup)
        <||> (consume RIGHT_PAREN "Expect ')' after arguments." >> return (reverse newBuildup))

primary :: MaybeT Planter Expr
primary = createLiteral <||> createGrouping <||> createVariable <||> noPrimary
 where
  createLiteral = Primary . literal <$> match (`elem` [FALSE, TRUE, NUMBER, STRING, NIL])
  createGrouping =
    Grouping
      <$> (match (== LEFT_PAREN) *> expression <* consume RIGHT_PAREN "Expect ')' after expression.")
  createVariable = Variable <$> match (== IDENTIFIER)
  noPrimary = parseError <$> match (const True) <*> pure "Expect expression." >>= addParseError
