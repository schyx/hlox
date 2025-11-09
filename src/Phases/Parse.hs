{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Phases.Parse (parse, TreeResult, expressionWrapper) where

import Control.Applicative (Alternative (many), (<|>))
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isNothing)
import Error (parseError)
import Parser (Parser (Parser, runParser))
import Phases.Expr
import Phases.Stmt
import Tokens

maxArgumentNumber :: Int
maxArgumentNumber = 255

type TreeResult = Either [String] [SomeStmt]

type Planter = Parser ([String], [Token])

makePlanter :: (([String], [Token]) -> Maybe (([String], [Token]), a)) -> Planter a
makePlanter = Parser

runPlanter :: Planter a -> (([String], [Token]) -> Maybe (([String], [Token]), a))
runPlanter = runParser

parse :: [Token] -> TreeResult
parse input =
  let start = ([], input)
      output =
        ( \maybeValue ->
            let value = fromMaybe undefined maybeValue
                errs = fst . fst $ value
                stmts = fromMaybe undefined $ sequenceA $ snd value
             in if null errs
                  then Right stmts
                  else Left errs
        )
          $ runPlanter (many planter) start
   in output

planter :: Planter (Maybe SomeStmt)
planter = do
  maybeStmt <- runMaybeT declaration
  when (isNothing maybeStmt) synchronize
  return maybeStmt

synchronize :: Planter ()
synchronize = makePlanter $ \(inErrs, inTokens) ->
  let syncTokens (token : tokens)
        | tokenType token == EOF = token : tokens
        | tokenType token == SEMICOLON = tokens
        | tokenType token `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN] = token : tokens
        | otherwise = syncTokens tokens
      syncTokens [] = error "Should not get empty in sync"
   in Just ((inErrs, syncTokens inTokens), ())

-- | Allows for choice on the Planter level rather than the MaybeT level
(<||>) :: MaybeT Planter a -> MaybeT Planter a -> MaybeT Planter a
MaybeT p1 <||> MaybeT p2 = MaybeT $ p1 <|> p2

infixl 3 <||>

-- | Checks that the first token satisfies the predicate
check :: (TokenType -> Bool) -> MaybeT Planter Token
check predicate = MaybeT $ makePlanter $ \(inErrs, inTokens) ->
  let firstToken = head inTokens
   in if predicate $ tokenType firstToken
        then Just ((inErrs, inTokens), Just firstToken)
        else Nothing

getFirstToken :: MaybeT Planter Token
getFirstToken = check $ const True

match :: (TokenType -> Bool) -> MaybeT Planter Token
match predicate = MaybeT $ makePlanter $ \(inErrs, inTokens) ->
  let firstToken = head inTokens
      restTokens = tail inTokens
   in if predicate $ tokenType firstToken
        then Just ((inErrs, restTokens), Just firstToken)
        else Nothing

consume :: TokenType -> String -> MaybeT Planter Token
consume desiredType message = MaybeT $ makePlanter $ \(inErrs, inTokens) ->
  let firstToken = head inTokens
      restTokens = tail inTokens
      errsIfFail = parseError firstToken message : inErrs
   in Just $
        if tokenType firstToken == desiredType
          then ((inErrs, restTokens), Just firstToken)
          else ((errsIfFail, inTokens), Nothing)

addParseError :: String -> MaybeT Planter a
addParseError errMessage = MaybeT $ makePlanter $ \(inErrs, tokens) ->
  Just ((errMessage : inErrs, tokens), Nothing)

addNonBlockingParseError :: String -> MaybeT Planter ()
addNonBlockingParseError errMessage = MaybeT $ makePlanter $ \(inErrs, tokens) ->
  Just ((errMessage : inErrs, tokens), Just ())

expressionWrapper :: [Token] -> Either [String] Expr
expressionWrapper =
  (\((errs, _), expr) -> if null errs then Right expr else Left errs)
    . fromMaybe undefined
    . runPlanter (fromMaybe undefined <$> runMaybeT expression)
    . ([],)

declaration :: MaybeT Planter SomeStmt
declaration =
  (SomeStmt <$> classDeclaration)
    <||> (SomeStmt <$> functionDeclaration)
    <||> (SomeStmt <$> variableDeclaration)
    <||> statement

classDeclaration :: MaybeT Planter (Stmt KClass)
classDeclaration =
  Class
    <$> (match (== CLASS) *> consume IDENTIFIER "Expect class name." <* consume LEFT_BRACE "Expect '{' before class body.")
    <*> (parseMethods <* consume RIGHT_BRACE "Expect '}' after class body.")
 where
  parseMethods = many $ createCallable "method"

functionDeclaration :: MaybeT Planter (Stmt KFunction)
functionDeclaration = match (== FUN) *> createCallable "function"

createCallable :: String -> MaybeT Planter (Stmt KFunction)
createCallable callableType = do
  name <-
    if callableType == "function"
      then consume IDENTIFIER $ "Expect " ++ callableType ++ " name."
      else matchMaybeT (`notElem` [RIGHT_BRACE, EOF])
  _ <- consume LEFT_PAREN $ "Expect '(' after " ++ callableType ++ " name."
  parameters <- getParameters
  _ <- consume LEFT_BRACE $ "Expect '{' before " ++ callableType ++ " body."
  body <- buildBlock []
  return $ Function name parameters body
 where
  getParameters = addParameters [] <||> endParameterList []
  addParameters buildup = do
    when (length buildup >= maxArgumentNumber) $ do
      firstToken <- getFirstToken
      let tooManyParamsMessage = parseError firstToken ("Can't have more than " ++ show maxArgumentNumber ++ " parameters.")
      addNonBlockingParseError tooManyParamsMessage
    parameter <- match (== IDENTIFIER)
    let newBuildup = parameter : buildup
    (match (== COMMA) >> addParameters newBuildup)
      <||> endParameterList newBuildup
  endParameterList buildup = consume RIGHT_PAREN "Expect ')' after parameters." $> reverse buildup
  matchMaybeT predicate = MaybeT $ makePlanter $ \(inErrs, inTokens) ->
    let firstToken = head inTokens
        restTokens = tail inTokens
     in Just $
          if predicate $ tokenType firstToken
            then ((inErrs, restTokens), Just firstToken)
            else ((inErrs, inTokens), Nothing)

variableDeclaration :: MaybeT Planter (Stmt KVar)
variableDeclaration = do
  variableName <- match (== VAR) *> consume IDENTIFIER "Expect variable name."
  assignVal variableName
    <||> noVal variableName
    <||> unexpectedToken
 where
  assignVal variableName =
    Var variableName <$> (match (== EQUAL) *> expression <* consume SEMICOLON "Expect ';' after variable declaration.")
  noVal variableName = Var variableName <$> (match (== SEMICOLON) $> Primary Nil)
  unexpectedToken =
    (parseError <$> match (const True) <*> pure "Expect ';' after variable declaration.")
      >>= addParseError

statement :: MaybeT Planter SomeStmt
statement =
  forStatement
    <||> (SomeStmt <$> ifStatement)
    <||> (SomeStmt <$> printStatement)
    <||> (SomeStmt <$> returnStatement)
    <||> (SomeStmt <$> whileStatement)
    <||> (SomeStmt <$> blockStatement)
    <||> (SomeStmt <$> expressionStatement)

forStatement :: MaybeT Planter SomeStmt
forStatement = do
  _ <- match (== FOR)
  _ <- consume LEFT_PAREN "Expect '(' after for."
  initializer <-
    (match (== SEMICOLON) >> pure Nothing)
      <||> (Just . SomeStmt <$> variableDeclaration)
      <||> (Just . SomeStmt <$> expressionStatement)
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
  desugarToWhile maybeInitializer maybeCondition maybeIncrement body =
    let newBody = case maybeIncrement of
          Nothing -> body
          Just increment -> SomeStmt . Block $ [body, SomeStmt $ Expression increment]
        newCondition = case maybeCondition of
          Nothing -> Primary $ Boolean True
          Just condition -> condition
        newStmt =
          let while = While newCondition newBody
           in case maybeInitializer of
                Nothing -> SomeStmt while
                Just initializer -> SomeStmt $ Block [initializer, SomeStmt while]
     in newStmt

ifStatement :: MaybeT Planter (Stmt KIf)
ifStatement = do
  _ <- match (== IF)
  _ <- consume LEFT_PAREN "Expect '(' after 'if'."
  expr <- expression
  _ <- consume RIGHT_PAREN "Expect ')' after if condition."
  ifBranch <- statement
  If expr ifBranch <$> getElseBranch
 where
  getElseBranch = (match (== ELSE) >> (Just <$> statement)) <||> pure Nothing

printStatement :: MaybeT Planter (Stmt KPrint)
printStatement = Print <$> (match (== PRINT) *> expression <* consume SEMICOLON "Expect ';' after expression.")

returnStatement :: MaybeT Planter (Stmt KReturn)
returnStatement = (match (== RETURN) >>= emptyReturn) <||> (match (== RETURN) >>= hasExprReturn)
 where
  emptyReturn returnToken = Return returnToken <$> (match (== SEMICOLON) $> Nothing)
  hasExprReturn returnToken = Return returnToken . Just <$> (expression <* consume SEMICOLON "Expect ';' after return value.")

whileStatement :: MaybeT Planter (Stmt KWhile)
whileStatement =
  While
    <$> (match (== WHILE) *> consume LEFT_PAREN "Expect '(' after while." *> expression)
    <*> (consume RIGHT_PAREN "Expect ')' after condition." *> statement)

blockStatement :: MaybeT Planter (Stmt KBlock)
blockStatement = Block <$> (match (== LEFT_BRACE) *> buildBlock [])

buildBlock :: [SomeStmt] -> MaybeT Planter [SomeStmt]
buildBlock buildup = rightBrace <||> endOfFile <||> moreStmts
 where
  rightBrace = match (== RIGHT_BRACE) $> reverse buildup
  endOfFile = check (== EOF) >>= addParseError . flip parseError "Expect '}' after block."
  moreStmts = declaration >>= buildBlock . (: buildup)

expressionStatement :: MaybeT Planter (Stmt KExpression)
expressionStatement =
  Expression
    <$> (check (/= EOF) *> expression <* consume SEMICOLON "Expect ';' after expression.")

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
        Get object name -> return $ Set object name value
        _ -> This equalSign <$ addNonBlockingParseError (parseError equalSign "Invalid assignment target.")
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
chainedOperator innerPlanter operatorTypes exprConstructor = do
  left <- innerPlanter
  ( do
      operator <- match (`elem` operatorTypes)
      chainedOperatorHelper left operator
    )
    <||> return left
 where
  chainedOperatorHelper left operator = do
    right <- innerPlanter
    let expr = exprConstructor left operator right
    ( do
        nextOperator <- match (`elem` operatorTypes)
        chainedOperatorHelper expr nextOperator
      )
      <||> return expr

unary :: MaybeT Planter Expr
unary = (Unary <$> match (`elem` [MINUS, BANG]) <*> unary) <||> call

call :: MaybeT Planter Expr
call = callOrGet <||> primary
 where
  callOrGet = do
    callee <- primary
    callOrGetLoop callee
  callOrGetLoop callee =
    ((Call callee <$> match (== LEFT_PAREN) <*> finishCall) >>= callOrGetLoop)
      <||> ( (match (== DOT) *> consume IDENTIFIER "Expect property name after '.'.")
              >>= callOrGetLoop . Get callee
           )
      <||> return callee
  finishCall =
    (check (/= RIGHT_PAREN) >> argsHelper [])
      <||> (consume RIGHT_PAREN "Expect ')' after arguments." >> pure [])
  argsHelper buildup = do
    when (length buildup >= maxArgumentNumber) $ do
      firstToken <- getFirstToken
      let tooManyArgsMsg = parseError firstToken ("Can't have more than " ++ show maxArgumentNumber ++ " arguments.")
      addNonBlockingParseError tooManyArgsMsg
    argument <- expression
    let newBuildup = argument : buildup
    (match (== COMMA) >> argsHelper newBuildup)
      <||> (consume RIGHT_PAREN "Expect ')' after arguments." >> return (reverse newBuildup))

primary :: MaybeT Planter Expr
primary = createThis <||> createLiteral <||> createGrouping <||> createVariable <||> noPrimary
 where
  createThis = This <$> match (== THIS)
  createLiteral = Primary . fromMaybe undefined . literal <$> match (`elem` [FALSE, TRUE, NUMBER, STRING, NIL])
  createGrouping = Grouping <$> (match (== LEFT_PAREN) *> expression <* consume RIGHT_PAREN "Expect ')' after expression.")
  createVariable = Variable <$> match (== IDENTIFIER)
  noPrimary = parseError <$> match (const True) <*> pure "Expect expression." >>= addParseError
