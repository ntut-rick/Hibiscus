{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}

module Hibiscus.CodeGen where

-- import qualified Data.Set as Set

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate, find)
import qualified Data.Map as Map
import Data.Maybe
import Data.STRef (newSTRef)
import Debug.Trace
import Hibiscus.Asm
import qualified Hibiscus.Ast as Ast
import Hibiscus.Lexer
import Hibiscus.Parser
import Hibiscus.Typing
import Control.Exception (handle)

import qualified Hibiscus.Type4plus as TI -- type infer

import Control.Monad.State.Lazy
import Data.Monoid (First(..), getFirst)

import Hibiscus.Util (foldMaplM, foldMaprM)


-- IDK why this is not imported from Asm
type ResultId = OpId

-- import Data.IntMap (fromList, foldlWithKey)

----- Instruction constructor helpers BEGIN -----

noReturnInstruction :: Ops -> Instruction
noReturnInstruction op = Instruction (Nothing, op)

returnedInstruction :: ResultId -> Ops -> Instruction
returnedInstruction id op = Instruction (Just id, op)

commentInstruction :: String -> Instruction
commentInstruction = noReturnInstruction . Comment

----- Instruction constructor helpers END -------

type Variable = (OpId, DataType)

type Uniform = (String, DataType, StorageClass, Int)

type Type = Ast.Type ()
type Dec = Ast.Dec (Range, Type)
type Expr = Ast.Expr (Range, Type)
type Argument = Ast.Argument (Range, Type)

getNameAndDType :: Argument -> (String, DataType)
getNameAndDType (Ast.Argument (_, t) (Ast.Name _ name)) = (BS.unpack name, typeConvert t)

type FunctionSignature = (DataType, [DataType]) -- return type, arguments

type Env = ([String], DataType) -- function name, function type

type ResultMap = Map.Map ResultType ExprReturn

-- type TypeInst = [Instruction]
-- type ConstInst = [Instruction]
-- type NameInst =[Instruction]
-- type UniformsInst =[Instruction]
type VariableInst= [Instruction]
type StackInst= [Instruction]


data Config = Config
  { capability :: Capability
  , extension :: String
  , memoryModel :: MemoryModel
  , addressModel :: AddressingModel
  , executionMode :: ExecutionMode
  , shaderType :: ExecutionModel
  , source :: (SourceLanguage, Int)
  , entryPoint :: String
  -- uniforms :: [Uniform] -- (name, type, position)
  }

data FunctionType
  = CustomFunction OpId String
  | TypeConstructor DataType -- function type constructor
  | TypeExtractor DataType [Int] -- function type decorator
  | OperatorFunction (Ast.Op (Range, Type))
  | FunctionFoldl -- base function
  | FunctionMap -- base function
  deriving (Show)

data ExprReturn
  = ExprResult Variable
  | ExprApplication FunctionType FunctionSignature [Variable] -- name function, arguments
  deriving (Show)

data ResultType
  = ResultDataType DataType -- done
  | ResultConstant Literal -- done
  | ResultVariable (Env, String, DataType) -- done
  | ResultVariableValue (Env, String, DataType) -- done
  | ResultFunction String FunctionSignature -- name return type, arguments
  | ResultCustom String -- done
  deriving (Show, Eq, Ord)

data LanxSt = LanxSt
  { idCount :: Int,
    idMap :: ResultMap,
    env :: Env,
    decs :: [Dec]
  }
  deriving (Show)

data HeaderFields = HeaderFields
  { capabilityInst :: Maybe Instruction
  , extensionInst :: Maybe Instruction
  , memoryModelInst :: Maybe Instruction
  , entryPointInst :: Maybe Instruction
  , executionModeInst :: Maybe Instruction
  , sourceInst :: Maybe Instruction
  }
  deriving (Show)

emptyHeaderFields =
  HeaderFields
    { capabilityInst = Nothing
    , extensionInst = Nothing
    , memoryModelInst = Nothing
    , entryPointInst = Nothing
    , executionModeInst = Nothing
    , sourceInst = Nothing
    }

fromHeaderFields :: HeaderFields -> [Instruction]
fromHeaderFields hf =
  [ commentInstruction "header fields"
  , fromJust (capabilityInst hf)
  , fromJust (extensionInst hf)
  , fromJust (memoryModelInst hf)
  , fromJust (entryPointInst hf)
  , fromJust (executionModeInst hf)
  , fromJust (sourceInst hf)
  ]

data FunctionInst= FunctionInst{
  begin :: [Instruction],
  parameter :: [Instruction],
  label :: [Instruction],
  variable :: [Instruction],
  body :: [Instruction],
  end :: [Instruction]
}deriving(Show)

data Instructions = Instructions
  { headerFields :: HeaderFields -- HACK: Maybe
  , nameFields :: [Instruction]
  , uniformsFields :: [Instruction]
  , typeFields :: [Instruction]
  , constFields :: [Instruction]
  , functionFields :: [FunctionInst] -- [function]
  }
  deriving (Show)

dtypeof :: Literal -> DataType
dtypeof (LBool _)  = bool
dtypeof (LUint _)  = uint32
dtypeof (LInt _)   = int32
dtypeof (LFloat _) = float32

-- FIXME: AFAIK, Instructions don’t actually form a Monoid.
--        However, since most folds are associative, I created this instance
--        to leverage Monoid for folding, making it less mentally taxing to work with.
--        USE WITH CAUTION.
instance Semigroup Instructions where
  (<>) = (+++)
instance Monoid Instructions where
  mempty = emptyInstructions

(+++) :: Instructions -> Instructions -> Instructions
(Instructions h1 n1 u1 t1 c1 f1) +++ (Instructions h2 n2 u2 t2 c2 f2) =
  Instructions (h1 `merge` h2) (n1 ++ n2) (u1 ++ u2) (t1 ++ t2) (c1 ++ c2) (f1 ++ f2)
 where
  mergeMaybe h Nothing = h
  mergeMaybe Nothing h = h
  mergeMaybe _ _ = error "fuck"


  merge :: HeaderFields -> HeaderFields -> HeaderFields
  merge hf1 hf2 =
    HeaderFields
      { capabilityInst = mergeMaybe (capabilityInst hf1) (capabilityInst hf2)
      , extensionInst = mergeMaybe (extensionInst hf1) (extensionInst hf2)
      , memoryModelInst = mergeMaybe (memoryModelInst hf1) (memoryModelInst hf2)
      , entryPointInst = mergeMaybe (entryPointInst hf1) (entryPointInst hf2)
      , executionModeInst = mergeMaybe (executionModeInst hf1) (executionModeInst hf2)
      , sourceInst = mergeMaybe (sourceInst hf1) (sourceInst hf2)
      }

-- FIXME: fucked up semantics

defaultConfig :: Config
defaultConfig =
  Config
    { capability = Shader
    , addressModel = Logical
    , memoryModel = GLSL450
    , source = (GLSL, 450)
    , shaderType = Fragment
    , executionMode = OriginUpperLeft
    , extension = "GLSL.std.450"
    , entryPoint = "main"
    -- uniforms = [("uv", vector2, Input, 0), ("outColor", vector4, Output, 0)]
    }

emptyInstructions :: Instructions
emptyInstructions =
  Instructions
    { headerFields = emptyHeaderFields -- FIXME:
    , nameFields = []
    , uniformsFields = []
    , typeFields = []
    , constFields = []
    , functionFields = []
    }

findResult' :: ResultType -> ResultMap -> Maybe ExprReturn
findResult' (ResultVariable ((envs, envType), name, varType)) viIdMap = -- find variable up to the mother env
  let param = (viIdMap, envType, name, varType)
   in getFirst . fst $ runState (foldMaplM (aux param) envs) []
  where
    aux :: (ResultMap, DataType, String, DataType) -> String -> State [String] (First ExprReturn)
    aux (viIdMap, envType, name, varType) env =
      do
        acc_env <- get
        let result = Map.lookup (ResultVariable ((acc_env++[env], envType), name, varType)) viIdMap
        put (acc_env ++ [env])
        return $ First result
findResult' (ResultVariableValue ((envs, envType), name, varType)) viIdMap = -- find variable up to the mother env
  let param = (viIdMap, envType, name, varType)
   in getFirst . fst $ runState (foldMaplM (aux param) envs) []
  where
    aux :: (ResultMap, DataType, String, DataType) -> String -> State [String] (First ExprReturn)
    aux (viIdMap, envType, name, varType) env =
      do
        acc_env <- get
        let result = Map.lookup (ResultVariableValue ((acc_env++[env], envType), name, varType)) viIdMap
        put (acc_env ++ [env])
        return $ First result
findResult' key viIdMap = Map.lookup key viIdMap

findResult :: LanxSt -> ResultType -> Maybe ExprReturn
findResult s key = 
  let viIdMap = idMap s -- very important idMap
   in findResult' key viIdMap

insertResultSt :: ResultType -> Maybe ExprReturn -> State LanxSt (ExprReturn)
insertResultSt key maybeER = do
  state <- get
  case findResult state key of
    Just existingResult -> return existingResult
    Nothing ->
      case maybeER of
        Nothing -> generateEntrySt key
        Just value -> do
          let newMap = Map.insert key value (idMap state)
          put $ state{idMap = newMap}
          return value

insertResult :: LanxSt -> ResultType -> Maybe ExprReturn -> (LanxSt, ExprReturn)
insertResult state key maybER =
  let (v, s') = runState (insertResultSt key maybER) state
   in (s', v)

generateEntrySt :: ResultType -> State LanxSt (ExprReturn)
generateEntrySt key = state $ \s ->
  let (newMap, newId, newCount) = generateEntry s key
      s' = s{idMap = newMap, idCount = newCount}
   in (newId, s')

-- Helper function to generate a new entry for the IdType
generateEntry :: LanxSt -> ResultType -> (ResultMap, ExprReturn, Int)
generateEntry state key =
  let currentMap = idMap state
      currentCount = idCount state
   in case key of
        ResultDataType t ->
          let var = (IdName (show t), t)
              result = ExprResult var
           in (Map.insert key result currentMap, result, currentCount)
        ResultConstant l ->
          let var = case l of
                LBool b -> (IdName ("bool_" ++ show b), DTypeBool)
                LUint i -> (IdName ("uint_" ++ show i), uint32)
                LInt i -> (IdName ("int_" ++ show i), int32)
                LFloat f -> (IdName ("float_" ++ map (\c -> if c == '.' then '_' else c) (show f)), float32)
              result = ExprResult var
           in (Map.insert key result currentMap, result, currentCount)
        ResultVariable ((envs, envType), name, varType) ->
          let var = (IdName (intercalate "_" (envs++[name])), varType)
              result = ExprResult var
           in (Map.insert key result currentMap, result, currentCount)
        ResultVariableValue ((envName, envType), s, varType) ->
          let var = (Id (currentCount + 1), varType)
              result = ExprResult var
           in (Map.insert key result currentMap, result, currentCount + 1)
        ResultFunction s (return, args) ->
          let t = DTypeFunction return args
              var = (IdName (intercalate "_" (s : map show args)), t)
              result = ExprResult var
           in (Map.insert key result currentMap, result, currentCount)

searchTypeId :: LanxSt -> DataType -> OpId
searchTypeId s dt = case findResult s (ResultDataType dt) of
  Just x -> case x of
    ExprResult (id, _) -> id
    _ -> error (show dt ++ " type not found")
  Nothing -> error (show dt ++ " type not found")

generateTypeSt_aux1 :: DataType -> State LanxSt (Instructions)
generateTypeSt_aux1 dType = do
  case dType of
    DTypeUnknown -> error "Unknown type"
    DTypeVoid    -> return emptyInstructions
    DTypeBool    -> return emptyInstructions
    DTypeInt _   -> return emptyInstructions
    DTypeUInt _  -> return emptyInstructions
    DTypeFloat _ -> return emptyInstructions
    DTypeVector _ baseType  -> do (_, inst) <- generateTypeSt baseType; return inst
    DTypeMatrix _ baseType  -> do (_, inst) <- generateTypeSt baseType; return inst
    DTypeArray _ baseType   -> do (_, inst) <- generateTypeSt baseType; return inst
    DTypePointer _ baseType -> do (_, inst) <- generateTypeSt baseType; return inst
    DTypeStruct _ fields              -> foldMaplM (\field -> do (_, inst) <- generateTypeSt field;                     return inst) fields
    DTypeFunction returnType argsType -> foldMaplM (\t ->     do (_, inst) <- generateTypeSt (DTypePointer Function t); return inst) (returnType : argsType)

generateTypeSt_aux2 :: DataType -> ResultId -> State LanxSt (Instructions)
generateTypeSt_aux2 dType typeId = state $ \state2 ->
  let
    searchTypeId' = searchTypeId state2

    -- IDK how this is possible, so I'll leave this magic in the box.
    (state3, inst3) = case dType of
      DTypeUnknown -> error "Unknown type"
      DTypeVoid                 -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeVoid)]})
      DTypeBool                 -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeBool)]})
      DTypeInt size             -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeInt size 0)]})
      DTypeUInt size            -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeInt size 1)]})
      DTypeFloat size           -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeFloat size)]})
      DTypeVector size baseType -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeVector (searchTypeId' baseType) size)]})
      DTypeMatrix col baseType  -> (state2, emptyInstructions{typeFields = [returnedInstruction typeId (OpTypeMatrix (searchTypeId' baseType) col)]})
      DTypeArray size baseType ->
        let (state4, constId, inst2) = generateConst state3 (LUint size)
            arrayInst = [returnedInstruction typeId (OpTypeArray (searchTypeId' baseType) constId)]
            inst3' = inst2{typeFields = typeFields inst2 ++ arrayInst}
          in (state4, inst3')
      DTypePointer storage DTypeVoid -> (state2, emptyInstructions)
      DTypePointer storage baseType ->
        let pointerInst = [returnedInstruction typeId (OpTypePointer storage (searchTypeId' baseType))]
            inst2' = emptyInstructions{typeFields = pointerInst}
          in (state2, inst2')
      DTypeStruct name baseTypes ->
        let structInst = [returnedInstruction typeId (OpTypeStruct (ShowList (map searchTypeId' baseTypes)))]
            inst2' = emptyInstructions{typeFields = structInst}
          in (state2, inst2')
      DTypeFunction returnType argTypes ->
        let functionInst = [returnedInstruction typeId (OpTypeFunction (searchTypeId' returnType) (ShowList (map (searchTypeId' . DTypePointer Function) argTypes)))]
            inst2' = emptyInstructions{typeFields = functionInst}
          in (state2, inst2')

    updatedState =
      state3
        { idCount = idCount state3
        , idMap = idMap state3
        }
  in
    (inst3, updatedState)

generateTypeSt :: DataType -> State LanxSt (OpId, Instructions)
generateTypeSt dType = do
  state <- get
  case findResult state (ResultDataType dType) of
    Just (ExprResult (typeId, _)) -> return (typeId, emptyInstructions)
    Nothing -> do
      inst  <- generateTypeSt_aux1 dType
      _er   <- insertResultSt (ResultDataType dType) Nothing
      let (ExprResult (typeId, _)) = _er
      inst3 <- generateTypeSt_aux2 dType typeId
      return (typeId, inst +++ inst3)

generateType :: LanxSt -> DataType -> (LanxSt, OpId, Instructions)
generateType state dType =
  let ((id, inst), s') = runState (generateTypeSt dType) state
   in (s', id, inst)

generateConstSt :: Literal -> State LanxSt (OpId, Instructions)
generateConstSt v = do
  let dtype = dtypeof v
  (typeId, typeInst) <- generateTypeSt dtype
  er <- insertResultSt (ResultConstant v) Nothing
  let (ExprResult (constId, dType)) = er
  let constInstruction = [returnedInstruction constId (OpConstant typeId v)]
  let inst = typeInst{constFields = constFields typeInst ++ constInstruction}
  return (constId, inst)

generateConst :: LanxSt -> Literal -> (LanxSt, OpId, Instructions)
generateConst state v =
  let ((id, inst), s') = runState (generateConstSt v) state
   in (s', id, inst)

generateNegOpSt :: Variable -> State LanxSt (Variable, StackInst)
generateNegOpSt v = state $ \state ->
  let id = Id (idCount state + 1)
      (e, t, typeId) = (fst v, snd v, searchTypeId state (snd v))
      state' =
        state
          { idCount = idCount state + 1
          }
      inst =
        case t of
          t
            | t == bool ->
                [returnedInstruction id (OpLogicalNot typeId e)]
            | t == int32 ->
                [returnedInstruction id (OpSNegate typeId e)]
            | t == float32 ->
                [returnedInstruction id (OpFNegate typeId e)]
          _ -> error ("not support neg of " ++ show t)
      result = (id, t)
   in ((result, inst), state')

generateNegOp :: LanxSt -> Variable -> (LanxSt, Variable, StackInst)
generateNegOp state v =
  let ((result, inst), s') = runState (generateNegOpSt v) state
   in (s', result, inst)

-- TODO: Unfinished Monad-ise
generateBinOpSt :: Variable -> Ast.Op (Range, Type) -> Variable -> State LanxSt (Variable, Instructions, StackInst)
generateBinOpSt v1 op v2 = state $ \s ->
  let (s', v, i, si) = generateBinOp s v1 op v2
   in ((v, i, si), s')

generateBinOp :: LanxSt -> Variable -> Ast.Op (Range, Type) -> Variable -> (LanxSt, Variable, Instructions, StackInst)
generateBinOp state v1 op v2 =
  let (e1, t1, typeId1) = (fst v1, snd v1, searchTypeId state (snd v1))
      (e2, t2, typeId2) = (fst v2, snd v2, searchTypeId state (snd v2))
      state' =
        state
          { idCount = idCount state + 1
          }
      id = Id (idCount state')
      (resultType, instruction) =
        case (t1, t2) of
          (t1, t2)
            | t1 == bool && t2 == bool ->
                case op of
                  Ast.Eq _ -> (bool, returnedInstruction (id) ( OpLogicalEqual typeId1 e1 e2))
                  Ast.Neq _ -> (bool, returnedInstruction (id) ( OpLogicalNotEqual typeId1 e1 e2))
                  Ast.And _ -> (bool, returnedInstruction (id) ( OpLogicalAnd typeId1 e1 e2))
                  Ast.Or _ -> (bool, returnedInstruction (id) ( OpLogicalOr typeId1 e1 e2))
            | t1 == int32 && t2 == int32 ->
                case op of
                  Ast.Plus _ -> (int32, returnedInstruction (id) ( OpIAdd typeId1 e1 e2))
                  Ast.Minus _ -> (int32, returnedInstruction (id) ( OpISub typeId1 e1 e2))
                  Ast.Times _ -> (int32, returnedInstruction (id) ( OpIMul typeId1 e1 e2))
                  Ast.Divide _ -> (int32, returnedInstruction (id) ( OpSDiv typeId1 e1 e2))
                  Ast.Eq _ -> (bool, returnedInstruction (id) ( OpIEqual typeId1 e1 e2))
                  Ast.Neq _ -> (bool, returnedInstruction (id) ( OpINotEqual typeId1 e1 e2))
                  Ast.Lt _ -> (bool, returnedInstruction (id) ( OpSLessThan typeId1 e1 e2))
                  Ast.Le _ -> (bool, returnedInstruction (id) ( OpSLessThanEqual typeId1 e1 e2))
                  Ast.Gt _ -> (bool, returnedInstruction (id) ( OpSGreaterThan typeId1 e1 e2))
                  Ast.Ge _ -> (bool, returnedInstruction (id) ( OpSGreaterThanEqual typeId1 e1 e2))
            | t1 == int32 && t2 == float32 -> error "Not implemented"
            | t1 == float32 && t2 == int32 -> error "Not implemented"
            | t1 == float32 && t2 == float32 ->
                case op of
                  Ast.Plus _ -> (float32, returnedInstruction (id) ( OpFAdd typeId1 e1 e2))
                  Ast.Minus _ -> (float32, returnedInstruction (id) ( OpFSub typeId1 e1 e2))
                  Ast.Times _ -> (float32, returnedInstruction (id) ( OpFMul typeId1 e1 e2))
                  Ast.Divide _ -> (float32, returnedInstruction (id) ( OpFDiv typeId1 e1 e2))
                  Ast.Eq _ -> (bool, returnedInstruction (id) ( OpFOrdEqual typeId1 e1 e2))
                  Ast.Neq _ -> (bool, returnedInstruction (id) ( OpFOrdNotEqual typeId1 e1 e2))
                  Ast.Lt _ -> (bool, returnedInstruction (id) ( OpFOrdLessThan typeId1 e1 e2))
                  Ast.Le _ -> (bool, returnedInstruction (id) ( OpFOrdLessThanEqual typeId1 e1 e2))
                  Ast.Gt _ -> (bool, returnedInstruction (id) ( OpFOrdGreaterThan typeId1 e1 e2))
                  Ast.Ge _ -> (bool, returnedInstruction (id) ( OpFOrdGreaterThanEqual typeId1 e1 e2))
            | t1 == t2 && (t1 == vector2 || t1 == vector3 || t1 == vector4) ->
                case op of
                  Ast.Plus _ -> (t1, returnedInstruction (id) ( OpFAdd typeId1 e1 e2))
                  Ast.Minus _ -> (t1, returnedInstruction (id) ( OpFSub typeId1 e1 e2))
                  Ast.Times _ -> (t1, returnedInstruction (id) ( OpFMul typeId1 e1 e2))
            | (t1 == vector2 || t1 == vector3 || t1 == vector4) && (t2 == int32 || t2 == float32) ->
                case op of
                  Ast.Times _ -> (vector2, returnedInstruction (id) ( OpVectorTimesScalar typeId1 e1 e2))
            | (t1 == int32 || t1 == float32) && (t2 == vector2 || t2 == vector3 || t2 == vector4) ->
                case op of
                  Ast.Times _ -> (vector2, returnedInstruction (id) ( OpVectorTimesScalar typeId1 e1 e2))
          _ -> error ("Not implemented" ++ show t1 ++ show op ++ show t2)
   in (state', (id, resultType), emptyInstructions, [instruction])

-- TODO: Unfinished Monad-ise
generateExprSt :: Expr -> State LanxSt (ExprReturn, Instructions,VariableInst, StackInst)
generateExprSt e = state $ \s ->
  let (s', er, inst, vi, si) = generateExpr s e
   in ((er, inst, vi, si), s')

generateExpr :: LanxSt -> Expr -> (LanxSt, ExprReturn, Instructions,VariableInst, StackInst)
generateExpr state expr =
  case expr of
    Ast.EBool _ x         -> let (s,v,i,si) = handleConst state (LBool x)  in (s, v, i, [], si)
    Ast.EInt _ x          -> let (s,v,i,si) = handleConst state (LInt x)   in (s, v, i, [], si)
    Ast.EFloat _ x        -> let (s,v,i,si) = handleConst state (LFloat x) in (s, v, i, [], si)
    Ast.EList _ l         -> let ((er,i,vi,si),s) = runState (handleArraySt l) state in (s,er,i,vi,si)
    Ast.EPar _ e          -> generateExpr state e
    Ast.EVar (_, t1) (Ast.Name (_, _) name) ->
                             let (s,v,i,si) = handleVar state t1 name in (s, v, i, [], si)
    Ast.EString _ _       -> error "String"
    Ast.EUnit _           -> error "Unit"
    Ast.EApp _ e1 e2      -> handleApp state e1 e2
    Ast.EIfThenElse _ e1 e2 e3 ->
                             handleIfThenElse state e1 e2 e3
    Ast.ENeg _ e          -> handleNeg state e
    Ast.EBinOp _ e1 op e2 -> handleBinOp state e1 op e2
    Ast.EOp _ _           -> let (s,v,i,si) = handleOp state expr in (s, v, i, [], si)
    Ast.ELetIn _ decs e   -> handleLetIn state decs e

-- TODO: Unfinished Monad-ise
handleLetInSt ::[Dec] -> Expr -> State LanxSt (ExprReturn, Instructions, VariableInst, StackInst)
handleLetInSt ds e = state $ \s ->
  let r@(s', er, inst, vi, si) = handleLetIn s ds e
   in ((er, inst, vi, si), s')

handleLetIn :: LanxSt -> [Dec] -> Expr -> (LanxSt, ExprReturn, Instructions, VariableInst, StackInst)
handleLetIn state decs e =
  let 
    (envs, envType )= env state
    state1 = state {env = (envs++["letIn"], envType)}
    ((inst, varInst, stackInst), state2) = runState (foldMaplM generateDecSt decs) state1
    (state3, result, inst1,varInst2 ,stackInst1) = generateExpr state2 e

    state4 = state3 {env = (envs, envType)}
  in (state4, result, inst +++ inst1,varInst++varInst2, stackInst ++ stackInst1)
    -- in error (show (findResult state2 (ResultVariableValue (env state2, "x", envType))))
    -- in error (show (idMap state2))
    -- in error (show decs)

handleConstSt :: Literal -> State LanxSt (ExprReturn, Instructions, StackInst)
handleConstSt lit =
  do
    (id, inst) <- generateConstSt lit
    return (ExprResult (id, dtypeof lit), inst,[])

handleConst :: LanxSt -> Literal -> (LanxSt, ExprReturn, Instructions, StackInst)
handleConst state lit =
  let ((er, inst, si), s') = runState (handleConstSt lit) state
   in (s', er, inst, si)

-- TODO: Unfinished Monad-ise
handleArraySt :: [Expr] -> State LanxSt (ExprReturn, Instructions, VariableInst, StackInst)
handleArraySt es =
  do
    let len = length es
    let makeAssociative (a,b,c,d) = ([a],b,c,d)
    (results, inst, var ,stackInst) <- foldMaplM (fmap makeAssociative .generateExprSt) es
    (typeId, typeInst) <- generateTypeSt (DTypeArray len DTypeUnknown)
    error "Not implemented array"

-- TODO: Unfinished Monad-ise
handleOpSt :: Expr -> State LanxSt (ExprReturn, Instructions, StackInst)
handleOpSt e = state $ \s ->
  let (s', er, i,si) = handleOp s e
   in ((er, i, si), s')

handleOp :: LanxSt -> Expr -> (LanxSt, ExprReturn, Instructions, StackInst)
handleOp state (Ast.EOp _ op) =
  let funcSign = case op of
        Ast.Plus _   -> (DTypeUnknown, [DTypeUnknown, DTypeUnknown])
        Ast.Minus _  -> (DTypeUnknown, [DTypeUnknown])
        Ast.Times _  -> (DTypeUnknown, [DTypeUnknown, DTypeUnknown])
        Ast.Divide _ -> (DTypeUnknown, [DTypeUnknown, DTypeUnknown])
        Ast.Eq _     -> (bool,         [DTypeUnknown, DTypeUnknown])
        Ast.Neq _    -> (bool,         [DTypeUnknown, DTypeUnknown])
        Ast.Lt _     -> (bool,         [DTypeUnknown, DTypeUnknown])
        Ast.Le _     -> (bool,         [DTypeUnknown, DTypeUnknown])
        Ast.Gt _     -> (bool,         [DTypeUnknown, DTypeUnknown])
        Ast.Ge _     -> (bool,         [DTypeUnknown, DTypeUnknown])
        Ast.And _    -> (DTypeUnknown, [DTypeUnknown, DTypeUnknown])
        Ast.Or _     -> (DTypeUnknown, [DTypeUnknown, DTypeUnknown])
   in (state, ExprApplication (OperatorFunction op) funcSign [], emptyInstructions, [])

-- TODO: Unfinished Monad-ise
handleVarFunctionSt :: String -> FunctionSignature -> State LanxSt (ExprReturn, Instructions, StackInst)
handleVarFunctionSt name fs = state $ \s ->
  let (s', er, i, si) = handleVarFunction s name fs
   in ((er, i, si), s')

handleVarFunction :: LanxSt -> String -> FunctionSignature -> (LanxSt, ExprReturn, Instructions,  StackInst)
handleVarFunction state name (returnType, args) =
  let result = findResult state (ResultFunction name (returnType, args))
   in case result of
        Just x -> (state, x, emptyInstructions, [])
        Nothing ->
          case (typeStringConvert name, returnType, args) of
            (Just x, return, args)
              | x == return -> (state, ExprApplication (TypeConstructor return) (return, args) [], emptyInstructions, [])
            (Nothing, return, args)
              | name == "foldl" -> (state, ExprApplication FunctionFoldl (return, args) [], emptyInstructions, [])
              | name == "map" -> (state, ExprApplication FunctionMap (return, args) [], emptyInstructions, [])
              | name == "extract_0" -> if length args == 1 && return == float32 then (state, ExprApplication (TypeExtractor returnType [0]) (return, args) [], emptyInstructions, []) else error (name ++ ":" ++ show args)
              | name == "extract_1" -> if length args == 1 && return == float32 then (state, ExprApplication (TypeExtractor returnType [1]) (return, args) [], emptyInstructions, []) else error (name ++ ":" ++ show args)
              | name == "extract_2" -> if length args == 1 && return == float32 then (state, ExprApplication (TypeExtractor returnType [2]) (return, args) [], emptyInstructions, []) else error (name ++ ":" ++ show args)
              | name == "extract_3" -> if length args == 1 && return == float32 then (state, ExprApplication (TypeExtractor returnType [3]) (return, args) [], emptyInstructions, []) else error (name ++ ":" ++ show args)
              | True ->
                  let dec = fromMaybe (error (name ++ show args)) (findDec (decs state) name Nothing)
                      ((id, inst1), state') = runState (generateFunctionSt emptyInstructions dec) state
                   in (state', ExprApplication (CustomFunction id name) (return, args) [], inst1, [])
                      -- error (show id ++ show (functionFields inst1))
            -- case findResult state (ResultFunction name ) of {}
            _ -> error "Not implemented function"

-- TODO: Unfinished Monad-ise
handleVarSt :: Type -> BS.ByteString -> State LanxSt (ExprReturn, Instructions, StackInst)
handleVarSt t1 n = state $ \s ->
  let (s', er,i,si) = handleVar s t1 n
   in ((er,i,si), s')

handleVar :: LanxSt -> Type-> BS.ByteString -> (LanxSt, ExprReturn, Instructions, StackInst)
handleVar state t1 n =
  let dType = typeConvert t1
      (state3, var, inst, stackInst) = case dType of
        DTypeFunction return args ->
          -- function variable
          handleVarFunction state (BS.unpack n) (return, args)
        _ ->
          -- value variable
          let maybeResult = findResult state (ResultVariableValue (env state, BS.unpack n, dType))
             in case maybeResult of
                  Just x -> (state, x, emptyInstructions, [])
                  Nothing ->
                    -- let ExprResult (varId, varType) = fromMaybe (error ("can find var:" ++ show (env state, BS.unpack n, dType))) (findResult state (ResultVariable (env state, BS.unpack n, dType)))
                    let ExprResult (varId, varType) = fromMaybe (error (show (idMap state))) (findResult state (ResultVariable (env state, BS.unpack n, dType)))
                        (state2, ExprResult (valueId, _)) = insertResult state (ResultVariableValue (env state, BS.unpack n, dType)) Nothing
                        inst = [returnedInstruction (valueId) ( OpLoad (searchTypeId state2 varType) varId)]
                    in (state2, ExprResult (valueId, varType), emptyInstructions, inst)
  --  in if n =="add" then error (show var) else (state3, var, inst, stackInst)
   in (state3, var, inst, stackInst)
  --  in if n =="add" then error (show var) else (state3, var, inst, stackInst)

-- TODO: Unfinished Monad-ise
handleAppSt :: Expr -> Expr -> State LanxSt (ExprReturn, Instructions, VariableInst, StackInst)
handleAppSt e1 e2 = state $ \s ->
  let (s', er,i,vi,si) = handleApp s e1 e2
   in ((er,i,vi,si), s')

handleApp :: LanxSt -> Expr -> Expr -> (LanxSt, ExprReturn, Instructions, VariableInst, StackInst)
handleApp state e1 e2 =
  let (state1, var1, inst1, varInst1,stackInst1) = generateExpr state e1
      (state2, var2, inst2, varInst2,stackInst2) = generateExpr state1 e2
      (state4, var3, inst3, varInst3,stackInst3) = case var1 of
        ExprApplication funcType (returnType, argTypes) args ->
          let args' = case var2 of
                ExprResult v -> args ++ [v] -- add argument
                _ -> error "Expected ExprResult"
              functionType = DTypeFunction returnType argTypes
           in case (length args', length argTypes) of
                (l, r) | l == r -> case funcType of
                  CustomFunction id s -> let ((er,is,vi,st), s') = runState (applyFunctionSt id returnType args') state2 in (s', er, is, vi, st)
                  TypeConstructor t -> let (s,v,i,si) = handleConstructor state2 t functionType args' in (s,v,i,[],si)
                  TypeExtractor t int -> let (s,v,i,si)=  handleExtract state2 t int (head args') in (s,v,i,[],si)
                  OperatorFunction op -> error "Not implemented" -- todo
                (l, r)
                  | l < r -> (state2, ExprApplication funcType (returnType, argTypes) args', emptyInstructions, [],[])
                (l, r) | l > r -> error "Too many arguments"
        _ -> error (show var1 ++ show var2)
   in (state4, var3, inst1 +++ inst2 +++ inst3, varInst1++varInst2++varInst3 ,stackInst1 ++ stackInst2 ++ stackInst3)

-- TODO: Unfinished Monad-ise
handleConstructorSt :: DataType -> DataType -> [Variable] -> State LanxSt (ExprReturn, Instructions, StackInst)
handleConstructorSt et ft args = state $ \s ->
  let (s', er,i,si) = handleConstructor s et ft args
   in ((er,i,si),s')

handleConstructor :: LanxSt -> DataType -> DataType -> [Variable] -> (LanxSt, ExprReturn, Instructions, StackInst)
handleConstructor state returnType functionType args =
  let (state1, typeId, inst) = generateType state returnType
      state2 = state1{idCount = idCount state1 + 1}
      returnId = Id (idCount state2) -- handle type convert
      stackInst = [returnedInstruction (returnId) ( OpCompositeConstruct typeId (ShowList (map fst args)))]
   in (state2, ExprResult (returnId, returnType), inst, stackInst)

handleExtract :: LanxSt -> DataType -> [Int] -> Variable -> (LanxSt, ExprReturn, Instructions, StackInst)
handleExtract state returnType i var =
  let (state1, typeId, inst) = generateType state returnType
      state2 = state1{idCount = idCount state1 + 1}
      returnId = Id (idCount state2)
      stackInst = [returnedInstruction (returnId) ( OpCompositeExtract typeId (fst var) (ShowList i))]
   in (state2, ExprResult (returnId, returnType), inst, stackInst)

applyFunctionSt_aux1 :: (OpId, (OpId, b)) -> State LanxSt ([(OpId, (OpId, b))], [Instruction], [Instruction])
applyFunctionSt_aux1 (typeId, t) =
  do
    s <- get
    let id = idCount s
    let varId = IdName ("param_" ++ show id)
    modify (\s -> s{idCount = idCount s + 1})
    return (
      [(varId, t)],
      [returnedInstruction (varId) (OpVariable typeId Function)],
      [noReturnInstruction (OpStore varId (fst t))])

applyFunctionSt :: OpId -> DataType -> [Variable] -> State LanxSt (ExprReturn, Instructions, VariableInst, StackInst)
applyFunctionSt id returnType args =
  do
    state0 <- get

    let makeAssociative (id, inst) = ([id], inst)
    (typeIds, inst1) <- foldMaprM (fmap makeAssociative . generateTypeSt . DTypePointer Function . snd) args

    modify (\s -> s{idCount = idCount s + 1})
    (vars, varInst, stackInst) <- foldMaplM applyFunctionSt_aux1 $ zip typeIds args

    state2 <- get
    let resultId = Id (idCount state2)
    let stackInst' = returnedInstruction (resultId) (OpFunctionCall (searchTypeId state0 returnType) id (ShowList (map fst vars)))
    -- (state', vars, typeInst, inst') = foldl (\(s, v, t, i) arg -> let (s', v', t', i') = functionPointer s arg in (s', v' : v, t ++ t', i ++ i')) (state, [], [], []) args
    -- state' = state {idCount = idCount state + 1}
    return (ExprResult (resultId, returnType), inst1, varInst, stackInst ++ [stackInst'])

handleIfThenElse :: LanxSt -> Expr -> Expr -> Expr -> (LanxSt, ExprReturn, Instructions, VariableInst, StackInst)
handleIfThenElse state e1 e2 e3 =
  let (state1, ExprResult var1, inst1,varInst1, stackInst1) = generateExpr state e1
      (state2, var2, inst2,varInst2, stackInst2) = generateExpr state1 e2
      (state3, var3, inst3,varInst3, stackInst3) = generateExpr state2 e3
      conditionId = case var1 of
        (id, DTypeBool) -> id
        _ -> error "Expected bool"
      id = idCount state3
      sInst1' = stackInst1 ++ [noReturnInstruction (OpBranchConditional conditionId (Id (id + 1)) (Id (id + 2)))]
      sInst2' = [returnedInstruction ((Id (id + 1))) ( OpLabel)] ++ stackInst2 ++ [noReturnInstruction ( OpBranch (Id (id + 3)))]
      sInst3' =
        [returnedInstruction ((Id (id + 2))) ( OpLabel)]
          ++ stackInst3
          ++ [noReturnInstruction (OpBranch (Id (id + 3)))]
          ++ [returnedInstruction ((Id (id + 3))) ( OpLabel)]
      state4 = state3{idCount = id + 3}
   in -- todo handle return variable
      (state3, var3, inst1 +++ inst2 +++ inst3,varInst1++varInst2++varInst3, sInst1' ++ sInst2' ++ sInst3')

-- error "Not implemented if then else"

handleNeg :: LanxSt -> Expr -> (LanxSt, ExprReturn, Instructions,VariableInst ,StackInst)
handleNeg state e =
  let (state1, ExprResult var, inst1,varInst1, stackInst1) = generateExpr state e
      (state2, var', stackInst2) = generateNegOp state1 var
   in (state2, ExprResult var', inst1,varInst1 ,stackInst1 ++ stackInst2)

handleBinOp :: LanxSt -> Expr -> Ast.Op (Range, Type) -> Expr -> (LanxSt, ExprReturn, Instructions,VariableInst, StackInst)
handleBinOp state e1 op e2 =
  let (state1, ExprResult var1, inst1,varInst1, stackInst1) = generateExpr state e1
      (state2, ExprResult var2, inst2,varInst2, stackInst2) = generateExpr state1 e2
      (state3, var3, inst3, stackInst3) = generateBinOp state2 var1 op var2
   in (state3, ExprResult var3, inst1 +++ inst2 +++ inst3,varInst1++varInst2, stackInst1 ++ stackInst2 ++ stackInst3)

generateDecSt :: Dec -> State LanxSt (Instructions, VariableInst, StackInst)
generateDecSt (Ast.DecAnno _ name t) = return mempty
generateDecSt (Ast.Dec (_, t) (Ast.Name (_, _) name) [] e) =
  do
    let varType = typeConvert t
    (typeId, inst1) <- generateTypeSt varType
    (result, inst2, varInst, stackInst) <- generateExprSt e
    state2 <- get
    _ <- insertResultSt (ResultVariable (env state2, BS.unpack name, varType)) (Just result)
    state3 <- get
    _ <- insertResultSt (ResultVariableValue (env state3, BS.unpack name, varType)) (Just result)
    return (inst1 +++ inst2, varInst, stackInst)

generateInitSt :: Config -> [Dec] -> State LanxSt (Instructions)
generateInitSt cfg decs = 
  do
    let startId = 0
    let headInstruction =
          HeaderFields
            { capabilityInst    = Just $ noReturnInstruction $ OpCapability (capability cfg)
            , extensionInst     = Just $ returnedInstruction (Id $ startId + 1) (OpExtInstImport (extension cfg))
            , memoryModelInst   = Just $ noReturnInstruction $ OpMemoryModel (addressModel cfg) (memoryModel cfg)
            , entryPointInst    = Nothing
            , executionModeInst = Just $ noReturnInstruction $ OpExecutionMode (IdName . entryPoint $ cfg) (executionMode cfg)
            , sourceInst        = Just $ noReturnInstruction $ uncurry OpSource (source cfg)
            }
    put $ LanxSt
            { idCount = startId + 1
            , idMap = Map.empty
            , env = ([entryPoint cfg], DTypeVoid)
            , decs = decs
            }
    _ <- insertResultSt (ResultCustom "ext ") (Just (ExprResult (Id 1, DTypeVoid))) -- ext
    let inst =
          Instructions
            { headerFields   = headInstruction
            , nameFields     = [commentInstruction "Name fields"]
            , uniformsFields = [commentInstruction "uniform fields"]
            , typeFields     = [commentInstruction "Type fields"]
            , constFields    = [commentInstruction "Const fields"]
            , functionFields = []
            }
    return inst

generateUniformsSt_aux1 :: (String, DataType, StorageClass, Int) -> State LanxSt (Instructions, [ResultId]) 
generateUniformsSt_aux1 (name, dType, storage, location) =
  do
    (typeId, inst1) <- generateTypeSt (DTypePointer storage dType)

    s1 <- get
    _er <- insertResultSt (ResultVariable (env s1, name, dType)) Nothing
    let ExprResult (id, _) = _er

    let variableInstruction = [returnedInstruction id (OpVariable typeId storage)]
    let nameInstruction     = [noReturnInstruction (OpName id name)]
    let uniformsInstruction = [noReturnInstruction (OpDecorate id (Location location))]

    let inst1' =
          inst1
            { nameFields     = nameFields inst1     ++ nameInstruction
            , uniformsFields = uniformsFields inst1 ++ uniformsInstruction
            , constFields    = constFields inst1    ++ variableInstruction
            }
    return (inst1', [id]) -- it's a anti-optimised move, but making less mentally taxing

generateUniformsSt :: Config -> [Argument] -> State LanxSt (Instructions)
generateUniformsSt cfg args =
  do
    let nntOfArgs = fmap getNameAndDType args
    let uniforms = fmap (\((n, t), i) -> (n, t, Input, i)) $ zip nntOfArgs [0..]
    let uniforms' = ("outColor", vector4, Output, 0) : uniforms -- todo handle custom output

    (inst, ids) <- foldMaplM generateUniformsSt_aux1 uniforms'

    let hf = trace "test" $ headerFields inst
    let hf' = hf{entryPointInst = Just $ noReturnInstruction (OpEntryPoint (shaderType cfg) (IdName (entryPoint cfg)) (entryPoint cfg) (ShowList ids))}
    let inst1 = inst{headerFields = hf'}

    return inst1

-- error (show (env state') ++ show uniforms')

generateFunctionParamSt :: [Ast.Argument (Range, Type)] -> State LanxSt (Instructions, [Instruction])
generateFunctionParamSt args =
  let 
    aux :: (String, DataType) -> State LanxSt (Instructions, Instruction)
    aux (name, dType) = do
      (typeId, inst1) <- generateTypeSt (DTypePointer Function dType)
      s' <- get
      er <- insertResultSt (ResultVariable (env s', name, dType)) Nothing
      let (ExprResult (id, _)) = er
      let paramInst = returnedInstruction (id) (OpFunctionParameter typeId)
      return (inst1, paramInst)
    makeAssociative (is, i) = (is, [i]) -- it's a anti-optimised move, but making less mentally taxing
  in do
    foldMaplM (fmap makeAssociative . aux) . fmap getNameAndDType $ args

-- error (show (env state') ++ show vars)

generateFunctionSt :: Instructions -> Dec -> State LanxSt (OpId, Instructions)
generateFunctionSt inst (Ast.DecAnno _ name t) = return $ (IdName "", inst)
generateFunctionSt inst (Ast.Dec (_, t) (Ast.Name (_, _) name) args e) =
  do
    state0 <- get
    let DTypeFunction returnType argsType = typeConvert t
    let functionType = DTypeFunction returnType argsType

    (typeId, inst1) <- generateTypeSt functionType
    modify (\s -> s{env = ([BS.unpack name], functionType)})

    er <- insertResultSt (ResultFunction (BS.unpack name) (returnType, argsType)) Nothing
    let ExprResult (funcId, funcType) = er

    (inst2, paramInst) <- generateFunctionParamSt args

    modify (\s -> s{idCount = idCount s + 1})

    state5 <- get
    let labelId = Id $ idCount state5

    (er, inst3, varInst, exprInst) <- generateExprSt e
    let ExprResult (resultId, _) = er

    modify (\s -> s{env = env state0})

    state7 <- get
    let returnTypeId = searchTypeId state7 returnType

    let funcInst = FunctionInst {
      begin     = [commentInstruction $ "function " ++ BS.unpack name, returnedInstruction funcId (OpFunction returnTypeId None typeId)],
      parameter = paramInst,
      label     = [returnedInstruction labelId OpLabel],
      variable  = varInst,
      body      = exprInst ++ [noReturnInstruction $ OpReturnValue resultId],
      end       = [noReturnInstruction OpFunctionEnd]
    }
    let inst4 = inst +++ inst1 +++ inst2 +++ inst3
    let inst5 = inst4{functionFields = functionFields inst4 ++ [funcInst]}
    return (funcId, inst5)

generateMainFunctionSt :: Instructions -> Config -> Dec -> State LanxSt Instructions
generateMainFunctionSt inst cfg (Ast.Dec (_, t) (Ast.Name (_, _) name) args e) =
  do
    let (returnType, argsType) = (DTypeVoid, [])
    let functionType = DTypeFunction returnType argsType

    (typeId, inst1) <- generateTypeSt functionType

    _er <- insertResultSt (ResultCustom "func ") (Just (ExprResult (IdName (BS.unpack name), functionType)))
    let ExprResult (funcId, _) = _er

    modify (\s -> s{idCount = idCount s + 1, env = ([BS.unpack name], functionType)})
    state3 <- get
    let labelId = Id (idCount state3)

    inst2 <- generateUniformsSt cfg args
    (_er, inst3, varInst ,exprInst) <- generateExprSt e
    let (ExprResult (resultId, _)) = _er
    state5 <- get
    let returnTypeId = searchTypeId state5 returnType

    let ExprResult (varId, _) = fromMaybe (error $ show $ env state5) (findResult state5 (ResultVariable (env state5, "outColor", vector4)))
    let saveInst = [noReturnInstruction (OpStore varId resultId)]

    let funcInst = FunctionInst {
      begin = [commentInstruction $ "function " ++ BS.unpack name, returnedInstruction (funcId) ( OpFunction returnTypeId None typeId)],
      parameter = [],
      label = [returnedInstruction (labelId) ( OpLabel)],
      variable = varInst,
      body = exprInst ++ [noReturnInstruction (OpReturn)],
      end = [noReturnInstruction (OpFunctionEnd)]
    }

    let inst4 = inst +++ inst1 +++ inst2 +++ inst3
    let inst5 = inst4{functionFields = functionFields inst4 ++ [funcInst]}
    return inst5


findDec' :: String -> Maybe FunctionSignature -> [Dec] -> Maybe Dec
findDec' name maybeFS = find' aux
  where
    aux = case maybeFS of
      Nothing ->
        ( \case
            Ast.Dec _ (Ast.Name _ n) _ _ -> n == BS.pack name
            _ -> False
        )
      Just (_, argTs) ->
        ( \case
            Ast.Dec _ (Ast.Name _ n) args' _ ->
              let argTs' = map (typeConvert . TI.getType) args'
              in n == BS.pack name && argTs == argTs
            _ -> False
        )
    find' :: (a -> Bool) -> [a] -> Maybe a
    find' p xs =
      case filter p xs of
        [v] -> Just v
        [] -> Nothing
        _ -> error "found multiple result, perhaps you want to use `find`"

-- search dec by name and function signature
findDec :: [Dec] -> String -> Maybe FunctionSignature -> Maybe Dec
findDec decs n mfs = findDec' n mfs decs

flattenFunctionInst :: FunctionInst -> [Instruction]
flattenFunctionInst func =
 let FunctionInst {begin, parameter, label, variable, body, end} = func
  in begin ++ parameter ++ label ++ variable ++ body ++ end

instructionsToString :: Instructions -> String
instructionsToString inst =
  do
    let code =
          fromHeaderFields (headerFields inst)
            ++ nameFields inst
            ++ uniformsFields inst
            ++ typeFields inst
            ++ constFields inst
            ++ concatMap flattenFunctionInst (functionFields inst)
    let codeText = intercalate "\n" (map show code)
    codeText

generate :: Config -> [Dec] -> Instructions
generate config decs =
  do
    let mainDec = fromJust $ findDec decs (entryPoint config) Nothing

    let nomatterState = undefined
    let (inst, initState) = runState (generateInitSt config decs) nomatterState
    let (inst', state') = runState (generateMainFunctionSt inst config mainDec) initState
    -- (state3, _, inst2) = generateType initState (DTypePointer Input vector2)

    let finalInst = inst'
    finalInst