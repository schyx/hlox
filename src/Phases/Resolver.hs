{-# LANGUAGE GADTs #-}

module Phases.Resolver (resolve, Locals (..)) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Error (resolveError)
import Phases.Expr (Expr (..))
import Phases.Stmt (SomeStmt (SomeStmt), Stmt (..))
import Tokens (Token (lexeme))

newtype Locals = Locals {resolverMap :: Map.Map Expr Int}

data FunctionType = NONE | FUNCTION | METHOD | INITIALIZER
  deriving (Eq)

data ClassType = NO_CLASS | CLASS
  deriving (Eq)

data ResolverType = ResolverType
  { errs :: [String]
  , currentResolverMap :: Map.Map Expr Int
  , scopes :: [Map.Map String Bool]
  , currentFunction :: FunctionType
  , currentClass :: ClassType
  }

addError :: Token -> String -> ResolverType -> ResolverType
addError tok errMsg rt = rt{errs = resolveError tok errMsg : errs rt}

beginScope :: ResolverType -> ResolverType
beginScope rt = rt{scopes = Map.empty : scopes rt}

endScope :: ResolverType -> ResolverType
endScope rt = rt{scopes = tail $ scopes rt}

declare :: Token -> ResolverType -> ResolverType
declare varName rt = case scopes rt of
  [] -> rt
  scope : rest ->
    case scope Map.!? lexeme varName of
      Just _ ->
        rt
          { errs =
              resolveError
                varName
                "Already a variable with this name in this scope."
                : errs rt
          , scopes = Map.insert (lexeme varName) False scope : rest
          }
      Nothing -> rt{scopes = Map.insert (lexeme varName) False scope : rest}

define :: Token -> ResolverType -> ResolverType
define varName rt = case scopes rt of
  [] -> rt
  scope : rest -> rt{scopes = Map.insert (lexeme varName) True scope : rest}

resolve :: [SomeStmt] -> Either [String] Locals
resolve inputStmts =
  let emptyResolverType =
        ResolverType
          { errs = []
          , currentResolverMap = Map.empty
          , scopes = []
          , currentFunction = NONE
          , currentClass = NO_CLASS
          }
      outputResolverType = foldl (flip resolveStmt) emptyResolverType inputStmts
   in if null $ errs outputResolverType
        then
          Right $
            Locals
              { resolverMap = currentResolverMap outputResolverType
              }
        else Left $ reverse $ errs outputResolverType

resolveName :: Expr -> Maybe Int -> ResolverType -> ResolverType
resolveName expr maybeDepth rt = case maybeDepth of
  Nothing -> rt
  Just depth -> rt{currentResolverMap = Map.insert expr depth $ currentResolverMap rt}

resolveLocal :: Expr -> Token -> ResolverType -> ResolverType
resolveLocal expr name rt = resolveName expr depth rt
 where
  depth :: Maybe Int
  depth = depthHelper 0 (scopes rt)
  depthHelper :: Int -> [Map.Map String Bool] -> Maybe Int
  depthHelper _ [] = Nothing
  depthHelper currentDepth (scope : rest) =
    case scope Map.!? lexeme name of
      Just _ -> Just currentDepth
      Nothing -> depthHelper (currentDepth + 1) rest

-- TODO: refactor to be in alphabetical order
resolveStmt :: SomeStmt -> ResolverType -> ResolverType
resolveStmt (SomeStmt (Expression expr)) = resolveExpr expr
resolveStmt (SomeStmt (Print expr)) = resolveExpr expr
resolveStmt (SomeStmt (Var varName varVal)) =
  define varName
    . resolveExpr varVal
    . declare varName
resolveStmt (SomeStmt (Block blockStmts)) =
  endScope
    . (\rt -> foldl (flip resolveStmt) rt blockStmts)
    . beginScope
resolveStmt (SomeStmt (If condition thenBranch maybeElseBranch)) =
  maybe id resolveStmt maybeElseBranch
    . resolveStmt thenBranch
    . resolveExpr condition
resolveStmt (SomeStmt (While condition body)) =
  resolveStmt body
    . resolveExpr condition
resolveStmt stmt@(SomeStmt (Function name _ _)) =
  resolveFunction stmt FUNCTION Nothing
    . define name
    . declare name
resolveStmt (SomeStmt (Return keyword returnValue)) =
  maybe id resolveExpr returnValue
    . checkFunctionType
 where
  checkFunctionType rt
    | currentFunction rt == NONE = addError keyword "Can't return from top-level code." rt
    | Just _ <- returnValue
    , currentFunction rt == INITIALIZER =
        addError keyword "Can't return a value from an initializer." rt
    | otherwise = rt
resolveStmt (SomeStmt (Class name methods)) =
  endScope
    . ( \rt ->
          foldl
            ( flip
                ( \method@(Function methodName _ _) ->
                    resolveFunction
                      (SomeStmt method)
                      (if lexeme methodName == "init" then INITIALIZER else METHOD)
                      $ Just CLASS
                )
            )
            rt
            methods
      )
    . (\rt -> rt{scopes = Map.insert "this" True (head $ scopes rt) : tail (scopes rt)})
    . beginScope
    . define name
    . declare name

resolveFunction :: SomeStmt -> FunctionType -> Maybe ClassType -> ResolverType -> ResolverType
resolveFunction (SomeStmt (Function _ params body)) ftype ctype resolverType =
  ( endScope
      . (\rt -> foldl (flip resolveStmt) rt body) -- resolve body
      . (\rt -> foldl (flip $ \tok -> define tok . declare tok) rt params) -- resolve params
      . beginScope
      $ resolverType{currentFunction = ftype, currentClass = fromMaybe (currentClass resolverType) ctype}
  )
    { currentFunction = currentFunction resolverType
    , currentClass = currentClass resolverType
    }
resolveFunction _ _ _ _ = error "called incorrectly"

resolveExpr :: Expr -> ResolverType -> ResolverType
resolveExpr expr@(Assign name value) = resolveLocal expr name . resolveExpr value
resolveExpr (Binary left _ right) = resolveExpr right . resolveExpr left
resolveExpr (Grouping expr) = resolveExpr expr
resolveExpr (Unary _ expr) = resolveExpr expr
resolveExpr expr@(Variable varTok) =
  let errorCheck rt = case scopes rt of
        [] -> rt
        scope : _ -> case scope Map.!? lexeme varTok of
          Nothing -> rt
          Just True -> rt
          Just False -> addError varTok "Can't read local variable in its own initializer." rt
   in resolveLocal expr varTok . errorCheck
resolveExpr (Primary _) = id
resolveExpr (OrExpr left _ right) =
  resolveExpr right
    . resolveExpr left
resolveExpr (AndExpr left _ right) =
  resolveExpr right
    . resolveExpr left
resolveExpr (Call caller _ args) =
  (\rt -> foldl (flip resolveExpr) rt args)
    . resolveExpr caller
resolveExpr (Get object _) = resolveExpr object
resolveExpr (Set object _ value) =
  resolveExpr value
    . resolveExpr object
resolveExpr expr@(This keyword) = resolveLocal expr keyword . checkClassType
 where
  checkClassType rt =
    if currentClass rt == NO_CLASS
      then addError keyword "Can't use 'this' outside of a class." rt
      else rt
