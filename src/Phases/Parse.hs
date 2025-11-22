{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Phases.Parse (parse, TreeResult, expressionWrapper) where

import Control.Applicative (Alternative (many), (<|>))
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Error (Error (..))
import Parser (Parser (Parser, runParser))
import Phases.Expr
import Phases.Stmt
import Tokens

maxArgumentNumber :: Int
maxArgumentNumber = 255

type TreeResult = Either [Error] [SomeStmt]

type Planter = Parser ([Error], [Token])

makePlanter :: (([Error], [Token]) -> Maybe (([Error], [Token]), a)) -> Planter a
makePlanter = Parser

runPlanter :: Planter a -> (([Error], [Token]) -> Maybe (([Error], [Token]), a))
runPlanter = runParser

parse :: [Token] -> TreeResult
parse input =
  let start = ([], input)
      output =
        ( \maybeValue ->
            let value = fromMaybe (([UnknownError "Got Nothing after running planter; shouldn't be possible?"], []), []) maybeValue
                (stmts, errs) = case sequenceA $ snd value of
                  Just statements -> (statements, fst . fst $ value)
                  Nothing -> ([], fst . fst $ value)
             in if null errs
                  then Right stmts
                  else Left $ reverse errs
        )
          $ runPlanter (many planter) start
   in output

planter :: Planter (Maybe SomeStmt)
planter = runMaybeT declaration

syncTokens :: [Token] -> ([Error], [Token])
syncTokens (token : tokens)
  | tokenType token == EOF = ([], token : tokens)
  | tokenType token == SEMICOLON = ([], tokens)
  | tokenType token `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN] = ([], token : tokens)
  | otherwise = syncTokens tokens
syncTokens [] =
  (
    [ UnknownError
        "Got empty in syncTokens; shouldn't be possible because we should always have EOF at end, and never get rid of EOF"
    ]
  , []
  )

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
      errsIfFail = ParseError firstToken message : inErrs
   in Just $
        if tokenType firstToken == desiredType
          then ((inErrs, restTokens), Just firstToken)
          else case syncTokens inTokens of
            ([], tokens) -> ((errsIfFail, tokens), Nothing)
            (errs, tokens) -> ((errs ++ errsIfFail, tokens), Nothing)

addParseError :: Error -> MaybeT Planter a
addParseError errMessage = MaybeT $ makePlanter $ \(inErrs, tokens) ->
  case syncTokens tokens of
    ([], syncedTokens) -> Just ((errMessage : inErrs, syncedTokens), Nothing)
    (syncErrs, syncedTokens) -> Just ((syncErrs ++ [errMessage] ++ inErrs, syncedTokens), Nothing)

addNonBlockingParseError :: Error -> MaybeT Planter ()
addNonBlockingParseError errMessage = MaybeT $ makePlanter $ \(inErrs, tokens) ->
  Just ((errMessage : inErrs, tokens), Just ())

expressionWrapper :: [Token] -> Either [Error] SomeExpr
expressionWrapper tokens =
  let planterStart = ([], tokens)
      ((outputErrs, _), outputExpr) = case runPlanter (runMaybeT expression) planterStart of
        Nothing -> (([UnknownError "expression should not return Nothing"], []), SomeExpr (Primary Nil))
        Just ((errs, toks), maybeExpr) -> case maybeExpr of
          Nothing -> ((UnknownError "Running planter should not lead to Nothing" : errs, toks), SomeExpr (Primary Nil))
          Just expr -> ((errs, toks), expr)
   in if null outputErrs then Right outputExpr else Left outputErrs

declaration :: MaybeT Planter SomeStmt
declaration =
  (SomeStmt <$> classDeclaration)
    <||> (SomeStmt <$> functionDeclaration)
    <||> (SomeStmt <$> variableDeclaration)
    <||> statement

classDeclaration :: MaybeT Planter (Stmt KClass)
classDeclaration =
  Class
    <$> (match (== CLASS) *> consume IDENTIFIER "Expect class name.")
    <*> (Just . Variable <$> (match (== LESS) *> consume IDENTIFIER "Expect superclass name.") <||> pure Nothing)
    <*> (consume LEFT_BRACE "Expect '{' before class body." *> parseMethods <* consume RIGHT_BRACE "Expect '}' after class body.")
 where
  parseMethods = MaybeT $ Parser $ \(inErrs, inToks) ->
    let output = runPlanter (runMaybeT $ many $ createCallable "method") (inErrs, inToks)
        ((outErrs, outToks), justMethods) = fromMaybe ((UnknownError "Cannot get nothing from many call" : outErrs, outToks), justMethods) output
        (finalErrs, methods) = case justMethods of
          Nothing -> (UnknownError "Got nothing from many call; shouldn't be possible?" : outErrs, [])
          Just inJustMethods -> (outErrs, inJustMethods)
     in if length outErrs == length inErrs
          then Just ((finalErrs, outToks), Just methods)
          else case syncTokens outToks of
            ([], tokens) -> Just ((finalErrs, tokens), Nothing)
            (syncErrs, tokens) -> Just ((syncErrs ++ finalErrs, tokens), Nothing)

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
      let tooManyParamsMessage = ParseError firstToken ("Can't have more than " ++ show maxArgumentNumber ++ " parameters.")
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
  noVal variableName = Var variableName <$> (match (== SEMICOLON) $> SomeExpr (Primary Nil))
  unexpectedToken =
    (ParseError <$> match (const True) <*> pure "Expect ';' after variable declaration.")
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
          Nothing -> SomeExpr $ Primary $ Boolean True
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
  endOfFile = check (== EOF) >>= addParseError . flip ParseError "Expect '}' after block."
  moreStmts = declaration >>= buildBlock . (: buildup)

expressionStatement :: MaybeT Planter (Stmt KExpression)
expressionStatement =
  Expression
    <$> (check (/= EOF) *> expression <* consume SEMICOLON "Expect ';' after expression.")

expression :: MaybeT Planter SomeExpr
expression = assignment

assignment :: MaybeT Planter SomeExpr
assignment = do
  expr <- orExpr
  ( do
      equalSign <- match (== EQUAL)
      value <- assignment
      case expr of
        (SomeExpr (Variable token)) -> return $ SomeExpr $ Assign token value
        (SomeExpr (Get object name)) -> return $ SomeExpr $ Set object name value
        _ -> SomeExpr (This equalSign) <$ addNonBlockingParseError (ParseError equalSign "Invalid assignment target.")
    )
    <||> return expr

toSomeExpr :: (SomeExpr -> Token -> SomeExpr -> Expr e) -> (SomeExpr -> Token -> SomeExpr -> SomeExpr)
toSomeExpr f left op right = SomeExpr $ f left op right

orExpr :: MaybeT Planter SomeExpr
orExpr = chainedOperator andExpr [OR] (toSomeExpr OrExpr)

andExpr :: MaybeT Planter SomeExpr
andExpr = chainedOperator equality [AND] (toSomeExpr AndExpr)

equality :: MaybeT Planter SomeExpr
equality = chainedOperator comparison [EQUAL_EQUAL, BANG_EQUAL] (toSomeExpr Binary)

comparison :: MaybeT Planter SomeExpr
comparison = chainedOperator term [LESS, LESS_EQUAL, GREATER, GREATER_EQUAL] (toSomeExpr Binary)

term :: MaybeT Planter SomeExpr
term = chainedOperator factor [PLUS, MINUS] (toSomeExpr Binary)

factor :: MaybeT Planter SomeExpr
factor = chainedOperator unary [SLASH, STAR] (toSomeExpr Binary)

chainedOperator ::
  MaybeT Planter SomeExpr ->
  [TokenType] ->
  (SomeExpr -> Token -> SomeExpr -> SomeExpr) ->
  MaybeT Planter SomeExpr
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

unary :: MaybeT Planter SomeExpr
unary = (SomeExpr <$> (Unary <$> match (`elem` [MINUS, BANG]) <*> unary)) <||> call

call :: MaybeT Planter SomeExpr
call = callOrGet <||> primary
 where
  callOrGet = do
    callee <- primary
    callOrGetLoop callee
  callOrGetLoop callee =
    ((Call callee <$> match (== LEFT_PAREN) <*> finishCall) >>= callOrGetLoop . SomeExpr)
      <||> ( (match (== DOT) *> consume IDENTIFIER "Expect property name after '.'.")
              >>= callOrGetLoop . SomeExpr . Get callee
           )
      <||> return callee
  finishCall =
    (check (/= RIGHT_PAREN) >> argsHelper [])
      <||> (consume RIGHT_PAREN "Expect ')' after arguments." >> pure [])
  argsHelper buildup = do
    when (length buildup >= maxArgumentNumber) $ do
      firstToken <- getFirstToken
      let tooManyArgsMsg = ParseError firstToken ("Can't have more than " ++ show maxArgumentNumber ++ " arguments.")
      addNonBlockingParseError tooManyArgsMsg
    arg <- expression
    let newBuildup = arg : buildup
    (match (== COMMA) >> argsHelper newBuildup)
      <||> (consume RIGHT_PAREN "Expect ')' after arguments." >> return (reverse newBuildup))

primary :: MaybeT Planter SomeExpr
primary = createThis <||> createLiteral <||> createGrouping <||> createSuper <||> createVariable <||> noPrimary
 where
  createThis = SomeExpr . This <$> match (== THIS)
  createLiteral = do
    tokenLiteral <- literal <$> match (`elem` [FALSE, TRUE, NUMBER, STRING, NIL])
    case tokenLiteral of
      Nothing -> addParseError $ UnknownError "Literal had not literal??"
      Just lit -> return $ SomeExpr $ Primary lit
  createGrouping = SomeExpr . Grouping <$> (match (== LEFT_PAREN) *> expression <* consume RIGHT_PAREN "Expect ')' after expression.")
  createSuper = SomeExpr <$> (Super <$> match (== SUPER) <*> (consume DOT "Expect '.' after 'super'." *> consume IDENTIFIER "Expect superclass method name."))
  createVariable = SomeExpr . Variable <$> match (== IDENTIFIER)
  noPrimary = ParseError <$> match (const True) <*> pure "Expect expression." >>= addParseError
