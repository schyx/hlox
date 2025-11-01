{-# LANGUAGE GADTs #-}

module Phases.Interpreter (
  Interpreter (Interpreter),
  EnvID (..),
  addFunction,
  defaultInterpreter,
  fromLiteral,
  createChildEnv,
  changeToParent,
  define,
  assignThis,
  assign,
  assignTok,
  lookupVariable,
  get,
  getAt,
  restoreRunningEnv,
  newInstance,
  setProperty,
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

data Interpreter = Interpreter
  { environmentTable :: Map.Map EnvID Environment
  , locals :: Map.Map Expr Int
  , currentEnvironment :: EnvID
  , nextEnvironmentId :: EnvID
  , nextInstanceId :: InstanceID
  , funcCounter :: FuncCounter
  }
  deriving (Show)

data Environment = Environment (Map.Map String Value) (Maybe EnvID)
  deriving (Show)

newtype EnvID = EnvID Int
  deriving (Show, Eq, Ord)

nextLargest :: EnvID -> EnvID
nextLargest (EnvID envId) = EnvID $ envId + 1

newtype InstanceID = InstanceID Int
  deriving (Show, Eq, Ord)

incrementInstanceId :: InstanceID -> InstanceID
incrementInstanceId (InstanceID instanceId) = InstanceID $ instanceId + 1

newtype FunctionID = FunctionID Int
  deriving (Show, Eq, Ord)

newtype FuncCounter = FuncCounter {getFuncCounter :: Map.Map ([Token], Token, String, Bool) FunctionID}
  deriving (Show)

addFunction ::
  [Token] ->
  Token ->
  String ->
  Bool ->
  Interpreter ->
  (Interpreter -> [Value] -> ExceptT String IO (Value, Interpreter)) ->
  Interpreter ->
  (Value, Interpreter)
addFunction params leftParen fname isInitializer definingInterp func interpreter =
  let fc = (getFuncCounter . funcCounter $ interpreter)
   in case fc Map.!? (params, leftParen, fname, isInitializer) of
        Nothing ->
          let newFc = FuncCounter $ Map.insert (params, leftParen, fname, isInitializer) (FunctionID 0) fc
           in ( VFunction params leftParen fname isInitializer definingInterp func $ FunctionID 0
              , interpreter{funcCounter = newFc}
              )
        Just (FunctionID count) ->
          let newFc = FuncCounter $ Map.insert (params, leftParen, fname, isInitializer) (FunctionID $ count + 1) fc
           in ( VFunction params leftParen fname isInitializer definingInterp func $ FunctionID $ count + 1
              , interpreter{funcCounter = newFc}
              )

defaultInterpreter :: Locals -> Interpreter
defaultInterpreter localVariables =
  let clockToken =
        MkToken
          { tokenType = IDENTIFIER
          , offset = 0
          , literal = Nothing
          , line = 0
          , lexeme = "clock"
          }
      clockFunc interp _ = do
        time <- liftIO getPOSIXTime
        return (VNumber (realToFrac time :: Double), interp)
      clock = VFunction [] clockToken "<native fn>" False undefined clockFunc (FunctionID 0)
      globalEnv = Environment (Map.fromList [("clock", clock)]) Nothing
      table = Map.fromList [(EnvID 0, globalEnv)]
   in Interpreter table (resolverMap localVariables) (EnvID 0) (EnvID 0) (InstanceID 0) (FuncCounter Map.empty)

createChildEnv :: Interpreter -> Interpreter
createChildEnv interpreter =
  let outputId = nextLargest . nextEnvironmentId $ interpreter
      emptyEnv = Environment Map.empty $ Just $ currentEnvironment interpreter
      newTable = Map.insert outputId emptyEnv $ environmentTable interpreter
   in interpreter
        { environmentTable = newTable
        , currentEnvironment = outputId
        , nextEnvironmentId = outputId
        }

changeToParent :: Interpreter -> Interpreter
changeToParent interpreter =
  let Environment _ parent = environmentTable interpreter Map.! currentEnvironment interpreter
   in case parent of
        Just parentEnvironmentId -> interpreter{currentEnvironment = parentEnvironmentId}
        Nothing -> error "can't change to parent when no parent"

define :: Interpreter -> Token -> Value -> Interpreter
define interpreter token value =
  let Environment envTable parent = environmentTable interpreter Map.! currentEnvironment interpreter
   in let definedTable = Map.insert (lexeme token) value envTable
          definedEnvironment = Environment definedTable parent
       in interpreter
            { environmentTable =
                Map.insert
                  (currentEnvironment interpreter)
                  definedEnvironment
                  (environmentTable interpreter)
            }

assignTok :: Interpreter -> Token -> Value -> Either String Interpreter
assignTok interpreter token value =
  case environmentTable interpreter Map.! currentEnvironment interpreter of
    (Environment envTable parent)
      | Map.member (lexeme token) envTable ->
          let assignedTable = Map.insert (lexeme token) value envTable
              assignedEnv = Environment assignedTable parent
           in Right
                interpreter
                  { environmentTable =
                      Map.insert
                        (currentEnvironment interpreter)
                        assignedEnv
                        (environmentTable interpreter)
                  }
      | Just pEnv <- parent -> do
          assignedInParent <- assignTok interpreter{currentEnvironment = pEnv} token value
          return $ interpreter{environmentTable = environmentTable assignedInParent}
      | Nothing <- parent ->
          Left $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

assignThis :: Value -> Interpreter -> Interpreter -> Interpreter
assignThis value (Interpreter _ _ thisInterp _ _ _) interpreter =
  let Environment variables parent = case environmentTable interpreter Map.!? thisInterp of
        Nothing -> error "here"
        Just e -> e
      env = Environment (Map.insert "this" value variables) parent
   in interpreter{environmentTable = Map.insert thisInterp env $ environmentTable interpreter}

assign :: Interpreter -> Expr -> Value -> Either String Interpreter
assign interpreter expr@(Assign name _) value =
  case locals interpreter Map.!? expr of
    Nothing -> assignGlobal interpreter name value
    Just distance -> assignAt interpreter distance name value
assign _ _ _ = error ""

assignAt :: Interpreter -> Int -> Token -> Value -> Either String Interpreter
assignAt interpreter distance name val =
  let ancestorId = ancestor distance interpreter $ currentEnvironment interpreter
      Environment envTable parent = fromMaybe (error "in assignAt") (environmentTable interpreter Map.!? ancestorId)
      newTable =
        Map.insert
          ancestorId
          (Environment (Map.insert (lexeme name) val envTable) parent)
          (environmentTable interpreter)
   in Right interpreter{environmentTable = newTable}

assignGlobal :: Interpreter -> Token -> Value -> Either String Interpreter
assignGlobal interpreter token value =
  let Environment globalTable pEnv = environmentTable interpreter Map.! EnvID 0
      newGlobalEnv = Environment (Map.insert (lexeme token) value globalTable) pEnv
   in case globalTable Map.!? lexeme token of
        Just _ ->
          Right
            interpreter
              { environmentTable = Map.insert (EnvID 0) newGlobalEnv $ environmentTable interpreter
              }
        Nothing -> Left $ runtimeError token $ "Undefined variable '" ++ lexeme token ++ "'."

lookupVariable :: Interpreter -> Token -> Expr -> Either String Value
lookupVariable interpreter name expr =
  case locals interpreter Map.!? expr of
    Nothing ->
      let Environment envTable _ = environmentTable interpreter Map.! EnvID 0
       in case envTable Map.!? lexeme name of
            Nothing -> Left (runtimeError name $ "Undefined variable '" ++ lexeme name ++ "'.")
            Just val -> Right val
    Just distance -> Right $ getAt interpreter distance $ lexeme name

getAt :: Interpreter -> Int -> String -> Value
getAt interpreter distance name =
  let Environment envTable _ =
        fromMaybe
          (error "in getAt")
          (environmentTable interpreter Map.!? ancestor distance interpreter (currentEnvironment interpreter))
   in case envTable Map.!? name of
        Nothing ->
          error $
            "getting "
              ++ name
              ++ " from envTable, distance is "
              ++ show distance
              ++ ", current is "
              ++ show (currentEnvironment interpreter)
              ++ "\n\n     interp is "
              ++ show interpreter
        Just val -> val

get :: Interpreter -> Token -> Either String Value
get interpreter token =
  let Environment envTable parent = environmentTable interpreter Map.! currentEnvironment interpreter
   in case envTable Map.!? lexeme token of
        Just val -> Right val
        Nothing ->
          case parent of
            Just pEnv -> get interpreter{currentEnvironment = pEnv} token
            Nothing -> Left $ runtimeError token ("Undefined variable '" ++ lexeme token ++ "'.")

ancestor :: Int -> Interpreter -> EnvID -> EnvID
ancestor distance interpreter childId
  | distance == 0 = childId
  | otherwise = ancestor (distance - 1) interpreter $
      case environmentTable interpreter Map.!? childId of
        Just (Environment _ parent) -> fromMaybe undefined parent
        Nothing -> error "in ancestor"

restoreRunningEnv :: Interpreter -> Interpreter -> Interpreter
restoreRunningEnv interpreter = setRunningEnv $ currentEnvironment interpreter

setRunningEnv :: EnvID -> Interpreter -> Interpreter
setRunningEnv envId interpreter = interpreter{currentEnvironment = envId}

newInstance :: String -> Map.Map String Value -> Interpreter -> (Value, Interpreter)
newInstance className classMethods interpreter =
  let iid = nextInstanceId interpreter
   in ( VInstance className Map.empty classMethods iid
      , interpreter{nextInstanceId = incrementInstanceId iid}
      )

setProperty :: InstanceID -> Token -> Value -> Interpreter -> Interpreter
setProperty iid name val interpreter =
  let mapFunc (VInstance className properties methods otherIid) =
        if iid == otherIid
          then VInstance className (Map.insert (lexeme name) val properties) methods otherIid
          else VInstance className (Map.map mapFunc properties) methods otherIid
      mapFunc other = other
      envMap (Environment vars pID) = Environment (Map.map mapFunc vars) pID
   in interpreter{environmentTable = Map.map envMap $ environmentTable interpreter}

-- TODO: use DataKinds to make instances contain methods
data Value
  = VNumber Double
  | VStr String
  | VBoolean Bool
  | VNil
  | VCall Int Token String
  | VFunction [Token] Token String Bool Interpreter (Interpreter -> [Value] -> ExceptT String IO (Value, Interpreter)) FunctionID
  | VClass String Int (Map.Map String Value)
  | VInstance String (Map.Map String Value) (Map.Map String Value) InstanceID

instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VStr s1) == (VStr s2) = s1 == s2
  (VBoolean b1) == (VBoolean b2) = b1 == b2
  VNil == VNil = True
  (VCall arity1 tok1 name1) == (VCall arity2 tok2 name2) = arity1 == arity2 && tok1 == tok2 && name1 == name2
  (VFunction params1 callee1 name1 isInit1 _ _ fid1) == (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    params1 == params2 && callee1 == callee2 && name1 == name2 && isInit1 == isInit2 && fid1 == fid2
  (VClass name1 _ _) == (VClass name2 _ _) = name1 == name2
  (VInstance _ _ _ iid1) == (VInstance _ _ _ iid2) = iid1 == iid2
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
  compare (VFunction params1 callee1 name1 isInit1 _ _ fid1) (VFunction params2 callee2 name2 isInit2 _ _ fid2) =
    compare (params1, callee1, name1, isInit1, fid1) (params2, callee2, name2, isInit2, fid2)
  compare VFunction{} other = case other of
    VClass{} -> LT
    VInstance{} -> LT
    _ -> GT
  compare (VClass name1 _ _) (VClass name2 _ _) = compare name1 name2
  compare VClass{} other = case other of
    VInstance{} -> LT
    _ -> GT
  compare (VInstance _ _ _ iid1) (VInstance _ _ _ iid2) = compare iid1 iid2
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
  show (VFunction _ _ s _ _ _ _) = s
  show (VCall _ _ s) = s
  show (VClass name _ _) = name
  show (VInstance name _ _ _) = name ++ " instance"

fromLiteral :: Literal -> Value
fromLiteral (Tokens.Number n) = VNumber n
fromLiteral (Tokens.Str s) = VStr s
fromLiteral (Tokens.Boolean b) = VBoolean b
fromLiteral Tokens.Nil = VNil
