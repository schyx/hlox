module Phases.Interpreter (
  Interpreter (Interpreter),
  EnvID (..),
  defaultInterpreter,
  fromLiteral,
  createChildEnv,
  changeToParent,
  define,
  assign,
  assignTok,
  lookupVariable,
  get,
  restoreRunningEnv,
  Value (..),
) where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error (runtimeError)
import Numeric (showFFloat)
import Phases.Expr (Expr (..))
import Phases.Resolver (Locals (resolverMap))
import Tokens

data Interpreter = Interpreter (Map.Map EnvID Environment) (Map.Map Expr Int) EnvID EnvID
  deriving (Show)

data Environment = Environment (Map.Map String Value) (Maybe EnvID)
  deriving (Show)

newtype EnvID = EnvID Int
  deriving (Show, Eq, Ord)

nextLargest :: EnvID -> EnvID
nextLargest (EnvID envId) = EnvID $ envId + 1

defaultInterpreter :: Locals -> Interpreter
defaultInterpreter locals =
  let clockToken =
        MkToken
          { tokenType = IDENTIFIER
          , offset = 0
          , literal = None
          , line = 0
          , lexeme = "clock"
          }
      clockFunc interp _ = do
        time <- liftIO getPOSIXTime
        return (VNumber (realToFrac time :: Double), interp)
      clock = VFunction [] clockToken "<native fn>" clockFunc
      globalEnv = Environment (Map.fromList [("clock", clock)]) Nothing
      table = Map.fromList [(EnvID 0, globalEnv)]
   in Interpreter table (resolverMap locals) (EnvID 0) (EnvID 0)

createChildEnv :: Interpreter -> Interpreter
createChildEnv (Interpreter table locals current largestId) =
  case table Map.!? current of
    Just _ ->
      let outputId = nextLargest largestId
          emptyEnv = Environment Map.empty $ Just current
          newTable = Map.insert outputId emptyEnv table
          outputInterpreter = Interpreter newTable locals outputId outputId
       in outputInterpreter
    Nothing -> error "parentId doesn't exist in Map"

changeToParent :: Interpreter -> Interpreter
changeToParent (Interpreter table locals current largestId) =
  case table Map.!? current of
    Just (Environment _ parent) ->
      case parent of
        Just pEnv -> Interpreter table locals pEnv largestId
        Nothing -> error "can't change to parent when no parent"
    Nothing -> error "parentId doesn't exist in Map"

define :: Interpreter -> Token -> Value -> Interpreter
define (Interpreter table locals current largestId) token value =
  case table Map.!? current of
    Just (Environment envTable parent) ->
      let envTable' = Map.insert (lexeme token) value envTable
          env' = Environment envTable' parent
          table' = Map.insert current env' table
       in Interpreter table' locals current largestId
    Nothing -> error "can't define in impossible env"

assignTok :: Interpreter -> Token -> Value -> Either String Interpreter
assignTok (Interpreter table locals current largestId) token value =
  case table Map.!? current of
    Just (Environment envTable parent)
      | Map.member (lexeme token) envTable ->
          let envTable' = Map.insert (lexeme token) value envTable
              env' = Environment envTable' parent
              table' = Map.insert current env' table
              interp' = Interpreter table' locals current largestId
           in Right interp'
      | Just pEnv <- parent -> do
          (Interpreter table' _ _ _) <- assignTok (Interpreter table locals pEnv largestId) token value
          return $ Interpreter table' locals current largestId
      | Nothing <- parent ->
          Left $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."
    Nothing -> error "Can't assign in impossible env"

assign :: Interpreter -> Expr -> Value -> Either String Interpreter
assign interp@(Interpreter _ locals _ _) expr@(Assign name _) value =
  case locals Map.!? expr of
    Nothing -> assignGlobal interp name value
    Just distance -> assignAt interp distance name value
assign _ _ _ = error ""

assignAt :: Interpreter -> Int -> Token -> Value -> Either String Interpreter
assignAt interp@(Interpreter table locals current largestId) distance name val =
  let ancestorId = ancestor distance interp current
      Environment envTable parent = fromMaybe (error "in assignAt") (table Map.!? ancestorId)
      newTable = Map.insert ancestorId (Environment (Map.insert (lexeme name) val envTable) parent) table
   in Right $ Interpreter newTable locals current largestId

assignGlobal :: Interpreter -> Token -> Value -> Either String Interpreter
assignGlobal (Interpreter table locals current largestId) token value =
  let Environment globalTable pEnv = table Map.! EnvID 0
      newGlobalEnv = Environment (Map.insert (lexeme token) value globalTable) pEnv
   in case globalTable Map.!? lexeme token of
        Just _ -> Right $ Interpreter (Map.insert (EnvID 0) newGlobalEnv table) locals current largestId
        Nothing -> Left $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

lookupVariable :: Interpreter -> Token -> Expr -> Either String Value
lookupVariable interp@(Interpreter table locals _ _) name expr =
  case locals Map.!? expr of
    Nothing ->
      let Environment envTable _ = table Map.! EnvID 0
       in case envTable Map.!? lexeme name of
            Nothing -> Left (runtimeError name $ "Undefined variable '" ++ lexeme name ++ "'.")
            Just val -> Right val
    Just distance -> getAt interp distance name

getAt :: Interpreter -> Int -> Token -> Either String Value
getAt interp@(Interpreter table _ current _) distance name =
  let Environment envTable _ = fromMaybe (error "in getAt") (table Map.!? ancestor distance interp current)
   in case envTable Map.!? lexeme name of
        Nothing -> error $ "getting " ++ lexeme name ++ " at line " ++ show (line name) ++ " from envTable, distance is " ++ show distance ++ ", current is " ++ show current ++ "\n\n     interp is " ++ show interp
        Just val -> Right val

get :: Interpreter -> Token -> Either String Value
get (Interpreter table locals current largestId) token =
  case table Map.!? current of
    Just (Environment envTable parent) ->
      case envTable Map.!? lexeme token of
        Just val -> Right val
        Nothing ->
          case parent of
            Just pEnv -> get (Interpreter table locals pEnv largestId) token
            Nothing -> Left $ runtimeError token ("Undefined variable '" ++ lexeme token ++ "'.")
    Nothing -> error "Can't get from impossible nev"

ancestor :: Int -> Interpreter -> EnvID -> EnvID
ancestor distance interp@(Interpreter table _ _ _) childId
  | distance == 0 = childId
  | otherwise = ancestor (distance - 1) interp $
      case table Map.!? childId of
        Just (Environment _ parent) -> fromMaybe undefined parent
        Nothing -> error "in ancestor"

restoreRunningEnv :: Interpreter -> Interpreter -> Interpreter
restoreRunningEnv (Interpreter _ _ env _) = setRunningEnv env

setRunningEnv :: EnvID -> Interpreter -> Interpreter
setRunningEnv envId (Interpreter table locals _ largest) = Interpreter table locals envId largest

data Value
  = VNumber Double
  | VStr String
  | VBoolean Bool
  | VNil
  | VCall Int Token String
  | VFunction [Token] Token String (Interpreter -> [Value] -> ExceptT String IO (Value, Interpreter))
  | VClass String
  | VInstance String

instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VStr s1) == (VStr s2) = s1 == s2
  (VBoolean b1) == (VBoolean b2) = b1 == b2
  VNil == VNil = True
  (VCall arity1 tok1 name1) == (VCall arity2 tok2 name2) = arity1 == arity2 && tok1 == tok2 && name1 == name2
  (VFunction params1 callee1 name1 _) == (VFunction params2 callee2 name2 _) =
    params1 == params2 && callee1 == callee2 && name1 == name2
  (VClass name1) == (VClass name2) = name1 == name2
  _ == _ = False

instance Ord Value where
  compare (VNumber n1) (VNumber n2) = compare n1 n2
  compare (VNumber _) _ = LT
  compare (VStr s1) (VStr s2) = compare s1 s2
  compare (VStr _) other = case other of
    VNumber _ -> GT
    _ -> LT
  compare (VBoolean b1) (VBoolean b2) = compare b1 b2
  compare (VBoolean _) other = case other of
    VNumber _ -> GT
    VStr _ -> GT
    _ -> LT
  compare VNil VNil = EQ
  compare VNil other = case other of
    VNumber _ -> GT
    VStr _ -> GT
    VBoolean _ -> GT
    _ -> LT
  compare (VCall arity1 tok1 name1) (VCall arity2 tok2 name2) = compare (arity1, tok1, name1) (arity2, tok2, name2)
  compare VCall{} other = case other of
    VFunction{} -> LT
    VClass{} -> LT
    _ -> GT
  compare (VFunction params1 callee1 name1 _) (VFunction params2 callee2 name2 _) =
    compare (params1, callee1, name1) (params2, callee2, name2)
  compare VFunction{} other = case other of
    VClass{} -> LT
    VInstance{} -> LT
    _ -> GT
  compare (VClass name1) (VClass name2) = compare name1 name2
  compare VClass{} other = case other of 
    VInstance{} -> LT
    _ -> GT
  compare (VInstance name1) (VInstance name2) = compare name1 name2
  compare VInstance{} _ = GT

instance Show Value where -- TODO: change literal to not include identifiers
  show (VNumber n) = formatNumber n
   where
    formatNumber :: Double -> String
    formatNumber x
      | isNegativeZero x = "-0"
      | x == fromInteger (round x) = show (round x :: Integer)
      | otherwise = showFFloat Nothing x ""
  show (VStr s) = s
  show (VBoolean b) = if b then "true" else "false"
  show VNil = "nil"
  show (VFunction _ _ s _) = s
  show (VCall _ _ s) = s
  show (VClass name) = name
  show (VInstance name) = name ++ " instance"

fromLiteral :: Literal -> Value
fromLiteral (Tokens.Number n) = VNumber n
fromLiteral (Tokens.Str s) = VStr s
fromLiteral (Tokens.Boolean b) = VBoolean b
fromLiteral Tokens.Nil = VNil
fromLiteral _ = error "can't convert from literal"
