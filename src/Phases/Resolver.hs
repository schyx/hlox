{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Phases.Resolver (resolve, Locals (..)) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Error (resolveError)
import Phases.Expr (Expr (..))
import Phases.Stmt (SomeStmt (SomeStmt), Stmt (..), StmtKind (..))
import Tokens (Token (lexeme))

newtype Locals = Locals {resolverMap :: Map.Map Expr Int}

data FunctionType = NONE | FUNCTION | METHOD | INITIALIZER
  deriving (Eq)

data ClassType = NO_CLASS | CLASS | SUBCLASS
  deriving (Eq)

data Resolver = Resolver
  { errors :: [String]
  , currentResolverMap :: Map.Map Expr Int
  , scopes :: [Map.Map String Bool]
  , currentFunction :: FunctionType
  , currentClass :: ClassType
  }

addError :: Token -> String -> Resolver -> Resolver
addError token errMessage resolver = resolver{errors = resolveError token errMessage : errors resolver}

beginScope :: Resolver -> Resolver
beginScope resolver = resolver{scopes = Map.empty : scopes resolver}

endScope :: Resolver -> Resolver
endScope resolver = resolver{scopes = tail $ scopes resolver}

declare :: Token -> Resolver -> Resolver
declare variableName resolver = case scopes resolver of
  [] -> resolver
  scope : rest ->
    case scope Map.!? lexeme variableName of
      Just _ ->
        resolver
          { errors =
              resolveError
                variableName
                "Already a variable with this name in this scope."
                : errors resolver
          , scopes = Map.insert (lexeme variableName) False scope : rest
          }
      Nothing -> resolver{scopes = Map.insert (lexeme variableName) False scope : rest}

define :: Token -> Resolver -> Resolver
define variableName resolver = case scopes resolver of
  [] -> resolver
  scope : rest -> resolver{scopes = Map.insert (lexeme variableName) True scope : rest}

resolve :: [SomeStmt] -> Either [String] Locals
resolve inputStmts =
  let emptyResolverType =
        Resolver
          { errors = []
          , currentResolverMap = Map.empty
          , scopes = []
          , currentFunction = NONE
          , currentClass = NO_CLASS
          }
      outputResolverType = foldl (flip resolveStmt) emptyResolverType inputStmts
   in if null $ errors outputResolverType
        then
          Right $
            Locals
              { resolverMap = currentResolverMap outputResolverType
              }
        else Left $ reverse $ errors outputResolverType

resolveName :: Expr -> Maybe Int -> Resolver -> Resolver
resolveName expr maybeDepth resolver = case maybeDepth of
  Nothing -> resolver
  Just depth -> resolver{currentResolverMap = Map.insert expr depth $ currentResolverMap resolver}

resolveLocal :: Expr -> Token -> Resolver -> Resolver
resolveLocal expr name resolver = resolveName expr depth resolver
 where
  depth :: Maybe Int
  depth = depthHelper 0 (scopes resolver)
  depthHelper :: Int -> [Map.Map String Bool] -> Maybe Int
  depthHelper _ [] = Nothing
  depthHelper currentDepth (scope : rest) =
    case scope Map.!? lexeme name of
      Just _ -> Just currentDepth
      Nothing -> depthHelper (currentDepth + 1) rest

resolveStmt :: SomeStmt -> Resolver -> Resolver
resolveStmt (SomeStmt (Block blockStmts)) =
  endScope
    . (\resolver -> foldl (flip resolveStmt) resolver blockStmts)
    . beginScope
resolveStmt (SomeStmt (Class name superclass methods)) =
  ( case superclass of
      Nothing -> id
      Just _ -> endScope
  )
    . endScope
    . ( \resolver ->
          foldl
            ( flip
                ( \method@(Function methodName _ _) ->
                    resolveFunction
                      method
                      (if lexeme methodName == "init" then INITIALIZER else METHOD)
                      $ Just
                      $ maybe CLASS (const SUBCLASS) superclass
                )
            )
            resolver
            methods
      )
    . (\resolver -> resolver{scopes = Map.insert "this" True (head $ scopes resolver) : tail (scopes resolver)})
    . beginScope
    . ( case superclass of
          Nothing -> id
          Just (Variable className) ->
            ( \resolver -> resolver{scopes = Map.insert "super" True (head $ scopes resolver) : tail (scopes resolver)}
            )
              . beginScope
              . resolveExpr (Variable className)
              . if lexeme name == lexeme className
                then addError className "A class can't inherit from itself."
                else id
          Just _ -> error "TODO: GADTs for Exprs"
      )
    . define name
    . declare name
resolveStmt (SomeStmt (Expression expr)) = resolveExpr expr
resolveStmt (SomeStmt stmt@(Function name _ _)) =
  resolveFunction stmt FUNCTION Nothing
    . define name
    . declare name
resolveStmt (SomeStmt (If condition thenBranch maybeElseBranch)) =
  maybe id resolveStmt maybeElseBranch
    . resolveStmt thenBranch
    . resolveExpr condition
resolveStmt (SomeStmt (Print expr)) = resolveExpr expr
resolveStmt (SomeStmt (Return keyword returnValue)) =
  maybe id resolveExpr returnValue
    . checkFunctionType
 where
  checkFunctionType resolver
    | currentFunction resolver == NONE = addError keyword "Can't return from top-level code." resolver
    | Just _ <- returnValue
    , currentFunction resolver == INITIALIZER =
        addError keyword "Can't return a value from an initializer." resolver
    | otherwise = resolver
resolveStmt (SomeStmt (Var variableName variableValue)) =
  define variableName
    . resolveExpr variableValue
    . declare variableName
resolveStmt (SomeStmt (While condition body)) =
  resolveStmt body
    . resolveExpr condition

resolveFunction :: Stmt KFunction -> FunctionType -> Maybe ClassType -> Resolver -> Resolver
resolveFunction (Function _ parameters body) functionType classType resolverType =
  ( endScope
      . (\resolver -> foldl (flip resolveStmt) resolver body) -- resolve body
      . (\resolver -> foldl (flip $ \token -> define token . declare token) resolver parameters) -- resolve params
      . beginScope
      $ resolverType{currentFunction = functionType, currentClass = fromMaybe (currentClass resolverType) classType}
  )
    { currentFunction = currentFunction resolverType
    , currentClass = currentClass resolverType
    }

resolveExpr :: Expr -> Resolver -> Resolver
resolveExpr (AndExpr left _ right) =
  resolveExpr right
    . resolveExpr left
resolveExpr expr@(Assign name value) = resolveLocal expr name . resolveExpr value
resolveExpr (Binary left _ right) = resolveExpr right . resolveExpr left
resolveExpr (Call caller _ arguments) =
  (\resolver -> foldl (flip resolveExpr) resolver arguments)
    . resolveExpr caller
resolveExpr (Get object _) = resolveExpr object
resolveExpr (Grouping expr) = resolveExpr expr
resolveExpr (OrExpr left _ right) =
  resolveExpr right
    . resolveExpr left
resolveExpr (Primary _) = id
resolveExpr (Set object _ value) =
  resolveExpr value
    . resolveExpr object
resolveExpr expr@(Super keyword _) =
  resolveLocal expr keyword
    . checkClassType
 where
  checkClassType resolver =
    case currentClass resolver of
      SUBCLASS -> resolver
      NO_CLASS -> addError keyword "Can't use 'super' outside of a class." resolver
      CLASS -> addError keyword "Can't use 'super' in a class with no superclass." resolver
resolveExpr expr@(This keyword) = resolveLocal expr keyword . checkClassType
 where
  checkClassType resolver =
    if currentClass resolver == NO_CLASS
      then addError keyword "Can't use 'this' outside of a class." resolver
      else resolver
resolveExpr (Unary _ expr) = resolveExpr expr
resolveExpr expr@(Variable variableToken) =
  let errorCheck resolver = case scopes resolver of
        [] -> resolver
        scope : _ -> case scope Map.!? lexeme variableToken of
          Nothing -> resolver
          Just True -> resolver
          Just False -> addError variableToken "Can't read local variable in its own initializer." resolver
   in resolveLocal expr variableToken . errorCheck
