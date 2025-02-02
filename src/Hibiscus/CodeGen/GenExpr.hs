{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant return" #-}

module Hibiscus.CodeGen.GenExpr where

-- import qualified Data.Set as Set

import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find, intercalate, tails)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (First (..), getFirst)
import Data.STRef (newSTRef)
import Debug.Trace (traceM, trace)
import qualified Hibiscus.Asm as Asm
import qualified Hibiscus.Ast as Ast
import Hibiscus.CodeGen.Constants (global)
import Hibiscus.CodeGen.Type.Builtin
import Hibiscus.CodeGen.Type.DataType (DataType)
import qualified Hibiscus.CodeGen.Type.DataType as DT
import Hibiscus.CodeGen.Types
import Hibiscus.CodeGen.Util
import qualified Hibiscus.Parsing.Lexer as L
import qualified Hibiscus.TypeInfer as TI
import Hibiscus.Util (foldMaplM, foldMaprM)
import Data.Type.Equality (apply)
import Control.Exception (handle)
import Control.Monad (when)
import Control.Arrow (ArrowApply(app))
import System.IO.Error (resourceVanishedErrorType)


----- Below are used by a lot of place

-- Helper function to generate a new entry for the IdType
-- used by findResultOrGenerateEntry
generateEntrySt :: ResultType -> State LanxSt ExprReturn
generateEntrySt key =
  let
    returnAndUpdateMap :: ExprReturn -> State LanxSt ExprReturn
    returnAndUpdateMap er = do
      modify (\s -> s{idMap = Map.insert key er $ idMap s})
      return er
    flattenTuples :: [(String, DataType)] -> [String]
    flattenTuples = concatMap (\(x, y) -> [x, show y])
   in
    case key of
      ResultDataType t -> do
        returnAndUpdateMap $ ExprResult (Asm.IdName $ show t, t)
      ResultConstant lit -> do
        let opid = Asm.IdName . idNameOf $ lit
        let litType = dtypeof lit
        returnAndUpdateMap $ ExprResult (opid, litType)
      ResultVariable (envs, name, varType) -> do
        let nameWithEnv = intercalate "_" (flattenTuples envs ++ [name])
        let opid = Asm.IdName nameWithEnv
        returnAndUpdateMap $ ExprResult (opid, varType)
      ResultVariableValue (_, _, varType) -> do
        varId <- nextOpId
        returnAndUpdateMap $ ExprResult (varId, varType)

-- TODO: unwrap this function to two function
insertResultSt :: ResultType -> Maybe ExprReturn -> State LanxSt ExprReturn
insertResultSt key maybeER = do
  traceM $ "[WARN] Deprecated: insertResultSt"
  state <- get
  case findResult state key of
    Just existingResult -> return existingResult
    Nothing ->
      case maybeER of
        Nothing -> generateEntrySt key
        Just value -> do
          insertResult' key value
          return value

findResultOrGenerateEntry :: ResultType -> State LanxSt ExprReturn
findResultOrGenerateEntry key =
  do
    state <- get
    case findResult state key of
      Just existingResult -> return existingResult
      Nothing -> generateEntrySt key

insertResult' :: ResultType -> ExprReturn -> State LanxSt ()
insertResult' key value =
  do
    viIdMap <- gets idMap
    if isNothing $ Map.lookup key viIdMap
      then
        modify (\s -> s{idMap = Map.insert key value viIdMap})
      else
        -- don't comment me out :'(
        error "duplicate key in idMap"

generateTypeSt_aux1 :: DataType -> State LanxSt Instructions
generateTypeSt_aux1 dType = do
  case dType of
    DT.DTypeUnknown -> error "Unknown type"
    DT.DTypeVoid -> return emptyInstructions
    DT.DTypeBool -> return emptyInstructions
    DT.DTypeInt _ -> return emptyInstructions
    DT.DTypeUInt _ -> return emptyInstructions
    DT.DTypeFloat _ -> return emptyInstructions
    DT.DTypeVector _ baseType -> fmap snd . generateTypeSt $ baseType
    DT.DTypeMatrix _ baseType -> fmap snd . generateTypeSt $ baseType
    DT.DTypeArray _ baseType -> fmap snd . generateTypeSt $ baseType
    DT.DTypePointer _ baseType -> fmap snd . generateTypeSt $ baseType
    DT.DTypeStruct _ fields -> foldMaplM (fmap snd . generateTypeSt) fields
    DT.DTypeFunction returnType argsType -> foldMaplM (fmap snd . generateTypeSt . DT.DTypePointer Asm.Function) (returnType : argsType)

generateTypeSt_aux2 :: DataType -> Asm.ResultId -> State LanxSt Instructions
generateTypeSt_aux2 dType typeId = state $ \state2 ->
  let
    searchTypeId' = searchTypeId state2

    -- IDK how this is possible, so I'll leave this magic in the box.
    (state3, inst3) = case dType of
      DT.DTypeUnknown -> error "Unknown type"
      DT.DTypeVoid -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeVoid typeId) Nothing]})
      DT.DTypeBool -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeBool typeId) Nothing]})
      DT.DTypeInt size -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeInt typeId size 0) Nothing]})
      DT.DTypeUInt size -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeInt typeId size 1) Nothing]})
      DT.DTypeFloat size -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeFloat typeId size) Nothing]})
      DT.DTypeVector size baseType -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeVector typeId (searchTypeId' baseType) size) Nothing]})
      DT.DTypeMatrix col baseType -> (state2, emptyInstructions{typeFields = [Asm.Inst (Asm.OpTypeMatrix typeId (searchTypeId' baseType) col) Nothing]})
      DT.DTypeArray size baseType ->
        let ((ExprResult (constId, _), inst2, _, _), state4) = runState (generateConstSt (Asm.LUint size)) state2 -- 💀 
            arrayInst = [Asm.Inst (Asm.OpTypeArray typeId (searchTypeId' baseType) constId) Nothing]
            inst3' = inst2{typeFields = typeFields inst2 ++ arrayInst}
         in (state4, inst3')
      DT.DTypePointer storage DT.DTypeVoid -> (state2, emptyInstructions)
      DT.DTypePointer storage baseType ->
        let pointerInst = [Asm.Inst (Asm.OpTypePointer typeId storage (searchTypeId' baseType)) Nothing]
            inst2' = emptyInstructions{typeFields = pointerInst}
         in (state2, inst2')
      DT.DTypeStruct name baseTypes ->
        let structInst = [Asm.Inst (Asm.OpTypeStruct typeId (map searchTypeId' baseTypes)) Nothing]
            inst2' = emptyInstructions{typeFields = structInst}
         in (state2, inst2')
      DT.DTypeFunction returnType argTypes ->
        let functionInst = [Asm.Inst (Asm.OpTypeFunction typeId (searchTypeId' returnType) (map (searchTypeId' . DT.DTypePointer Asm.Function) argTypes)) Nothing]
            inst2' = emptyInstructions{typeFields = functionInst}
         in (state2, inst2')

    updatedState =
      state3
        { idCount = idCount state3
        , idMap = idMap state3
        }
   in
    (inst3, updatedState)

-- used by a lot of place
generateTypeSt :: DataType -> State LanxSt (Asm.OpId, Instructions)
generateTypeSt dType = do
  state <- get
  case findResult state (ResultDataType dType) of
    Just (ExprResult (typeId, _)) -> return (typeId, emptyInstructions)
    Nothing -> do
      inst <- generateTypeSt_aux1 dType
      _er <- findResultOrGenerateEntry (ResultDataType dType)
      let (ExprResult (typeId, _)) = _er
      inst3 <- generateTypeSt_aux2 dType typeId
      return (typeId, inst +++ inst3)

----- Below are NegOp/BinOp lookup maps (I might wrong)

-- used by generateExprSt (Ast.ENeg _ e)
generateNegOpSt :: Variable -> State LanxSt (Variable, StackInst)
generateNegOpSt v@(e, t) =
  do
    id <- nextOpId
    typeId <- gets (\s -> searchTypeId s t)
    let asmop =
          case t of
            t
              | t == DT.bool -> Asm.OpLogicalNot id typeId e
              | t == DT.int32 -> Asm.OpSNegate id typeId e
              | t == DT.float32 -> Asm.OpFNegate id typeId e
            _ -> error ("not support neg of " ++ show t)
    let inst = Asm.Inst asmop Nothing
    let result = (id, t)
    return (result, [inst])

-- used by generateExprSt (Ast.EBinOp _ e1 op e2)
generateBinOpSt :: Variable -> Ast.Op (L.Range, Type) -> Variable -> State LanxSt (Variable, Instructions, StackInst)
generateBinOpSt v1@(e1, t1) op v2@(e2, t2) =
  do
    typeId1 <- gets (\s -> searchTypeId s t1)
    typeId2 <- gets (\s -> searchTypeId s t2)
    (boolId, inst) <- generateTypeSt DT.DTypeBool
    id <- nextOpId
    let (resultType, instruction) =
          case (t1, t2) of
            (t1, t2)
              | t1 == DT.bool && t2 == DT.bool ->
                  case op of
                    Ast.Eq _ -> (DT.bool, Asm.Inst (Asm.OpLogicalEqual id boolId e1 e2) Nothing)
                    Ast.Neq _ -> (DT.bool, Asm.Inst (Asm.OpLogicalNotEqual id boolId e1 e2) Nothing)
                    Ast.And _ -> (DT.bool, Asm.Inst (Asm.OpLogicalAnd id boolId e1 e2) Nothing)
                    Ast.Or _ -> (DT.bool, Asm.Inst (Asm.OpLogicalOr id boolId e1 e2) Nothing)
              | t1 == DT.int32 && t2 == DT.int32 ->
                  case op of
                    Ast.Plus _ -> (DT.int32, Asm.Inst (Asm.OpIAdd id typeId1 e1 e2) Nothing)
                    Ast.Minus _ -> (DT.int32, Asm.Inst (Asm.OpISub id typeId1 e1 e2) Nothing)
                    Ast.Times _ -> (DT.int32, Asm.Inst (Asm.OpIMul id typeId1 e1 e2) Nothing)
                    Ast.Divide _ -> (DT.int32, Asm.Inst (Asm.OpSDiv id typeId1 e1 e2) Nothing)
                    Ast.Eq _ -> (DT.bool, Asm.Inst (Asm.OpIEqual id boolId e1 e2) Nothing)
                    Ast.Neq _ -> (DT.bool, Asm.Inst (Asm.OpINotEqual id boolId e1 e2) Nothing)
                    Ast.Lt _ -> (DT.bool, Asm.Inst (Asm.OpSLessThan id boolId e1 e2) Nothing)
                    Ast.Le _ -> (DT.bool, Asm.Inst (Asm.OpSLessThanEqual id boolId e1 e2) Nothing)
                    Ast.Gt _ -> (DT.bool, Asm.Inst (Asm.OpSGreaterThan id boolId e1 e2) Nothing)
                    Ast.Ge _ -> (DT.bool, Asm.Inst (Asm.OpSGreaterThanEqual id boolId e1 e2) Nothing)
              | t1 == DT.int32 && t2 == DT.float32 -> error "Not implemented"
              | t1 == DT.float32 && t2 == DT.int32 -> error "Not implemented"
              | t1 == DT.float32 && t2 == DT.float32 ->
                  case op of
                    Ast.Plus _ -> (DT.float32, Asm.Inst (Asm.OpFAdd id typeId1 e1 e2) Nothing)
                    Ast.Minus _ -> (DT.float32, Asm.Inst (Asm.OpFSub id typeId1 e1 e2) Nothing)
                    Ast.Times _ -> (DT.float32, Asm.Inst (Asm.OpFMul id typeId1 e1 e2) Nothing)
                    Ast.Divide _ -> (DT.float32, Asm.Inst (Asm.OpFDiv id typeId1 e1 e2) Nothing)
                    Ast.Eq _ -> (DT.bool, Asm.Inst (Asm.OpFOrdEqual id boolId e1 e2) Nothing)
                    Ast.Neq _ -> (DT.bool, Asm.Inst (Asm.OpFOrdNotEqual id boolId e1 e2) Nothing)
                    Ast.Lt _ -> (DT.bool, Asm.Inst (Asm.OpFOrdLessThan id boolId e1 e2) Nothing)
                    Ast.Le _ -> (DT.bool, Asm.Inst (Asm.OpFOrdLessThanEqual id boolId e1 e2) Nothing)
                    Ast.Gt _ -> (DT.bool, Asm.Inst (Asm.OpFOrdGreaterThan id boolId e1 e2) Nothing)
                    Ast.Ge _ -> (DT.bool, Asm.Inst (Asm.OpFOrdGreaterThanEqual id boolId e1 e2) Nothing)
              | t1 == t2 && (t1 == DT.vector2 || t1 == DT.vector3 || t1 == DT.vector4) ->
                  case op of
                    Ast.Plus _ -> (t1, Asm.Inst (Asm.OpFAdd id typeId1 e1 e2) Nothing)
                    Ast.Minus _ -> (t1, Asm.Inst (Asm.OpFSub id typeId1 e1 e2) Nothing)
                    Ast.Times _ -> (t1, Asm.Inst (Asm.OpFMul id typeId1 e1 e2) Nothing)
              | (t1 == DT.vector2 || t1 == DT.vector3 || t1 == DT.vector4) && (t2 == DT.int32 || t2 == DT.float32) ->
                  case op of
                    Ast.Times _ -> (DT.vector2, Asm.Inst (Asm.OpVectorTimesScalar id typeId1 e1 e2) Nothing)
              | (t1 == DT.int32 || t1 == DT.float32) && (t2 == DT.vector2 || t2 == DT.vector3 || t2 == DT.vector4) ->
                  case op of
                    Ast.Times _ -> (DT.vector2, Asm.Inst (Asm.OpVectorTimesScalar id typeId1 e1 e2) Nothing)
            _ -> error ("Not implemented" ++ show t1 ++ show op ++ show t2)
    return ((id, resultType), inst, [instruction])

----- Below are directly used by generateExprSt

-- fold aux used by generateExprSt (Ast.ELetIn _ decs e)
generateDecSt :: Dec -> State LanxSt VeryImportantTuple
generateDecSt (Ast.DecAnno _ name t) = error "Infer type should not pass DecAnno"
generateDecSt (Ast.Dec (_, t) (Ast.Name _ name) [] e) =
  do
    let varType = typeConvert t
    (typeId, inst1) <- generateTypeSt varType
    (result, inst2, varInst, stackInst) <- generateExprSt e
    env_state3 <- gets env 
    -- idMap should not have insert key
    insertResult' (ResultVariableValue (env_state3, BS.unpack name, varType)) result
    return (result,inst1 +++ inst2, varInst, stackInst)
generateDecSt (Ast.Dec (r1, t) (Ast.Name r2 name) args e) =
  do
    env_state0 <- gets env
    let functionType = typeConvert t
    let DT.DTypeFunction returnType argsType = functionType

    (typeId, inst1) <- generateTypeSt functionType
    modify (\s -> s{env = [global]})
    env_state1 <- gets env

    let funcId = Asm.IdName (intercalate "_" $ BS.unpack name : map show argsType)
    let result = ExprApplication (BaseFunction (CustomFunction funcId (BS.unpack name))) (returnType, argsType) []
    er <- insertResultSt (ResultVariableValue (env_state1, BS.unpack name, functionType)) (Just result)


    state <- get

    modify (\s -> s{env = global : [(BS.unpack name, functionType)]})

    (inst2, paramInst) <- generateFunctionParamSt args

    labelId <- nextOpId

    (er, inst3, varInst, exprInst) <- generateExprSt e >>= applyExpr
    traceM ("generateFunctionSt " ++ show er)
    let ExprResult (resultId, _) = er

    modify (\s -> s{env = env_state0})

    returnTypeId <- gets (\s -> searchTypeId s returnType)

    let funcInst =
          FunctionInst
            { begin = [commentInstruction $ "function " ++ BS.unpack name, Asm.Inst (Asm.OpFunction funcId returnTypeId Asm.Pure typeId) Nothing]
            , parameter = paramInst
            , label = [Asm.Inst (Asm.OpLabel labelId) Nothing]
            , variable = varInst
            , body = exprInst ++ [noReturnInstruction $ Asm.OpReturnValue resultId]
            , end = [noReturnInstruction Asm.OpFunctionEnd]
            }
    let inst4 = inst1 +++ inst2 +++ inst3
    let inst5 = inst4{functionFields = functionFields inst4 ++ [funcInst]}
    return (result, inst5, [], [])

-- this function is aux of generateFunctionSt
generateFunctionParamSt :: [Ast.Argument (L.Range, Type)] -> State LanxSt (Instructions, [Asm.Instruction])
generateFunctionParamSt args =
  let
    aux :: (String, DataType) -> State LanxSt (Instructions, Asm.Instruction)
    aux (name, dType) = do
      (typeId, inst1) <- generateTypeSt (DT.DTypePointer Asm.Function dType)
      env_s' <- gets env
      _er <- findResultOrGenerateEntry (ResultVariable (env_s', name, dType))
      let (ExprResult (id, _)) = _er
      traceM ("generateFunctionParamSt " ++ show (env_s', name, dType))
      let paramInst = Asm.Inst (Asm.OpFunctionParameter id typeId) Nothing
      return (inst1, paramInst)
    makeAssociative (is, i) = (is, [i]) -- it's a anti-optimised move, but making less mentally taxing
   in
    do
      foldMaplM (fmap makeAssociative . aux) . fmap getNameAndDType $ args


-- used by generateExprSt literals
generateConstSt :: Asm.Literal -> State LanxSt VeryImportantTuple
generateConstSt v = do
  state <- get
  case findResult state (ResultConstant v) of
    Just x -> return (x, mempty, [], [])
    Nothing ->
        do
          let dtype = dtypeof v
          (typeId, typeInst) <- generateTypeSt dtype
          er <- findResultOrGenerateEntry (ResultConstant v)
          let (ExprResult (constId, dType)) = er
          let constInstruction = case v of
                Asm.LInt _ -> [Asm.Inst (Asm.OpConstant constId typeId v) Nothing]
                Asm.LUint _ -> [Asm.Inst (Asm.OpConstant constId typeId v) Nothing]
                Asm.LFloat _ -> [Asm.Inst (Asm.OpConstant constId typeId v) Nothing]
                Asm.LBool t_f | t_f==True -> [Asm.Inst (Asm.OpConstantTrue constId typeId) Nothing]
                Asm.LBool t_f | t_f==False-> [Asm.Inst (Asm.OpConstantFalse constId typeId) Nothing]
                _ -> error ("Not supported"++ show v)
          let inst = typeInst{typeFields = typeFields typeInst ++ constInstruction}
          return (ExprResult (constId, dtype), inst, [], [])

----- Below are use by generateExprSt (Ast.EApp _ e1 e2)

applyFunctionSt_aux1 :: (Asm.OpId, (Asm.OpId, b)) -> State LanxSt ([(Asm.OpId, (Asm.OpId, b))], VariableInst, StackInst)
applyFunctionSt_aux1 (typeId, t) =
  do
    varId <- nextOpIdName (\i -> "param_" ++ show i)
    return
      ( [(varId, t)]
      , [Asm.Inst (Asm.OpVariable varId typeId Asm.Function) Nothing]
      , [noReturnInstruction (Asm.OpStore varId (fst t))]
      )

functionCallSt :: Asm.OpId -> DataType -> [Variable] -> State LanxSt VeryImportantTuple
functionCallSt id returnType args =
  do
    searchTypeId_state0_returnType <- gets (\s -> searchTypeId s returnType) -- FIXME: please rename this
    let makeAssociative (id, inst) = ([id], inst)
    (typeIds, inst1) <- foldMaprM (fmap makeAssociative . generateTypeSt . DT.DTypePointer Asm.Function . snd) args

    (vars, varInst, stackInst) <- foldMaplM applyFunctionSt_aux1 $ zip typeIds args

    resultId <- nextOpId
    let stackInst' = Asm.Inst (Asm.OpFunctionCall resultId searchTypeId_state0_returnType id (map fst vars)) Nothing
    -- (state', vars, typeInst, inst') = foldl (\(s, v, t, i) arg -> let (s', v', t', i') = functionPointer s arg in (s', v' : v, t ++ t', i ++ i')) (state, [], [], []) args
    -- state' = state {idCount = idCount state + 1}
    return (ExprResult (resultId, returnType), inst1, varInst, stackInst ++ [stackInst'])

handleConstructorSt :: DataType -> [Variable] -> State LanxSt VeryImportantTuple
handleConstructorSt returnType args =
  do
    (typeId, inst) <- generateTypeSt returnType
    returnId <- nextOpId -- handle type convert
    let stackInst = [Asm.Inst (Asm.OpCompositeConstruct returnId typeId (map fst args)) Nothing]
    return (ExprResult (returnId, returnType), inst, [], stackInst)

handleExtractSt :: DataType -> [Int] -> Variable -> State LanxSt VeryImportantTuple
handleExtractSt returnType is var@(opId, _) =
  do
    (typeId, inst) <- generateTypeSt returnType
    returnId <- nextOpId
    let stackInst = [Asm.Inst (Asm.OpCompositeExtract returnId typeId opId is) Nothing]
    return (ExprResult (returnId, returnType), inst, [], stackInst)

----- Below are stateless

handleOp' :: Ast.Op (L.Range, Type) -> ExprReturn
handleOp' op =
  let funcSign = case op of
        Ast.Plus _ -> (DT.DTypeUnknown, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Minus _ -> (DT.DTypeUnknown, [DT.DTypeUnknown])
        Ast.Times _ -> (DT.DTypeUnknown, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Divide _ -> (DT.DTypeUnknown, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Eq _ -> (DT.bool, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Neq _ -> (DT.bool, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Lt _ -> (DT.bool, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Le _ -> (DT.bool, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Gt _ -> (DT.bool, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Ge _ -> (DT.bool, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.And _ -> (DT.DTypeUnknown, [DT.DTypeUnknown, DT.DTypeUnknown])
        Ast.Or _ -> (DT.DTypeUnknown, [DT.DTypeUnknown, DT.DTypeUnknown])
   in ExprApplication (BaseFunction (OperatorFunction op)) funcSign []

appendApplication :: ExprReturn -> [Variable] -> ExprReturn
appendApplication (ExprApplication funcType funcSign args) arg = 
  ExprApplication funcType funcSign (args ++ arg)
appendApplication (ExprResult x) [] = 
  ExprResult x
appendApplication (ExprResult x) arg =
  error ("appendApplication: " ++ show x ++ " " ++ show arg)




handleIfElseSt_aux1::[Variable] -> VeryImportantTuple -> State LanxSt VeryImportantTuple
handleIfElseSt_aux1  args (er,inst,varInst,stackInst) =
  do
    -- traceM ("handleIfElseSt_aux1 " ++ show er)
    let er' = appendApplication er args
    applyExpr (er',inst,varInst,stackInst)

handleIfElseSt :: Expr ->Expr ->Expr -> [Variable]-> State LanxSt VeryImportantTuple
handleIfElseSt condE thenE elseE args=
  do
    (condEr,inst1,varInst1,stackInstCond) <- generateExprSt condE >>= applyExpr
    (thenEr,inst2,varInst2,stackInstThen) <- generateExprSt thenE >>= handleIfElseSt_aux1 args
    (elseEr,inst3,varInst3,stackInstElse) <- generateExprSt elseE >>= handleIfElseSt_aux1 args

    let ExprResult (conditionId, _) = condEr

    let ExprResult (thenResultId, returnType) = thenEr
    let ExprResult (elseResultId, _rt       ) = elseEr
    let varType = DT.DTypePointer Asm.Function returnType

    (varTypeId, inst4) <- generateTypeSt varType
    (varValueTypeId, inst5) <- generateTypeSt returnType

    opid_plus_1 <- nextOpId
    let (Asm.Id opid_plus_1_num) = opid_plus_1
    thenLabelId <- nextOpId
    elseLabelId <- nextOpId
    ifThenElseEndLabelId <- nextOpId
    finalReturnId <- nextOpId

    envs <- gets env
    _er <- insertResultSt (ResultVariable (envs, "ifThen" ++ show (opid_plus_1_num), returnType)) Nothing
    let (ExprResult (varId, _)) = _er

    let sInst' =
      -- sInst1'
          stackInstCond ++
          [noReturnInstruction $ Asm.OpSelectionMerge (ifThenElseEndLabelId) Asm.None] ++
          [noReturnInstruction $ Asm.OpBranchConditional conditionId (thenLabelId) (elseLabelId)] ++ 
      -- sInst2'
          [commentInstruction "then branch"] ++
          [Asm.Inst (Asm.OpLabel thenLabelId) Nothing] ++
          stackInstThen ++
          [noReturnInstruction $ Asm.OpStore varId thenResultId] ++
          [noReturnInstruction $ Asm.OpBranch (ifThenElseEndLabelId)] ++
      -- sInst3'
          [commentInstruction "else branch"] ++
          [Asm.Inst (Asm.OpLabel elseLabelId) Nothing] ++
          stackInstElse ++
          [noReturnInstruction $ Asm.OpStore varId elseResultId] ++
          [noReturnInstruction $ Asm.OpBranch (ifThenElseEndLabelId)] ++
      --
          [commentInstruction "merged branch"] ++
          [Asm.Inst (Asm.OpLabel ifThenElseEndLabelId) Nothing] ++
          [Asm.Inst (Asm.OpLoad finalReturnId varValueTypeId varId) Nothing]
    let varInst = varInst1 ++ varInst2 ++ varInst3 ++ [Asm.Inst (Asm.OpVariable varId varTypeId Asm.Function) Nothing]
    return (
      ExprResult (finalReturnId, returnType),
      inst1 +++ inst2 +++ inst3 +++ inst4 +++ inst5,
      varInst,
      sInst')

applyExpr :: VeryImportantTuple -> State LanxSt VeryImportantTuple
applyExpr (var1, inst1, varInst1, stackInst1) =
  do
    (var2,inst2,varInst2,stackInst2) <-  case var1 of
        ExprResult x ->  return (ExprResult x, mempty,[],[]) -- already evaluated
        ExprApplication funcType (returnType,argsType) args -> case funcType of
            BaseFunction (CustomFunction id s) -> functionCallSt id returnType args
            BaseFunction (TypeConstructor t )-> handleConstructorSt t args
            BaseFunction (TypeExtractor t int )-> handleExtractSt t int (head args)
            BaseFunction (OperatorFunction op )-> error "Not implemented" -- TODO:
            BaseFunction FunctionFoldl -> error "Not implemented" -- TODO: eval foldl gen array length
            IfElseApplication condEr thenEr elseEr -> handleIfElseSt condEr thenEr elseEr args
            _ -> error "Not implemented"
    

    return (var2, inst1 +++ inst2, varInst1 ++ varInst2, stackInst1 ++ stackInst2)

generateExprSt :: Expr -> State LanxSt VeryImportantTuple
generateExprSt (Ast.EPar _ e) = generateExprSt e
generateExprSt (Ast.EBool _ x) = generateConstSt (Asm.LBool x)
generateExprSt (Ast.EInt _ x) = generateConstSt (Asm.LInt x)
generateExprSt (Ast.EFloat _ x) = generateConstSt (Asm.LFloat x)
generateExprSt (Ast.EList _ es) =
  do
    let len = length es
    let makeAssociative (a, b, c, d) = ([a], b, c, d)
    (results, inst, var, stackInst) <- foldMaplM (fmap makeAssociative . generateExprSt) es
    (typeId, typeInst) <- generateTypeSt (DT.DTypeArray len DT.DTypeUnknown)
    error "Not implemented array" -- TODO: EList
generateExprSt (Ast.EVar (_, t1) (Ast.Name _ bsname)) =
  let
    name = BS.unpack bsname
    dType = typeConvert t1
  in
  do
    state <- get
    let k = ResultVariableValue (env state, name, dType)
    case findResult state k of
      Just er -> return (er, mempty, [], [])
      Nothing -> 
          case findDec (decs state) name dType of
            Just dec -> do
              old_env <- gets env
              -- set global env
              modify (\s -> s{env = [global]})
              result <- generateDecSt dec
              modify (\s -> s{env = old_env})
              return result
            Nothing -> case getBulitinFunctionType name of 
              Just funcTy -> do
                let exprReturn = ExprApplication (BaseFunction funcTy) (DT.DTypeUnknown, []) []
                return (exprReturn, mempty, [], [])
              Nothing -> do
                let er = fromMaybe (error (show (name, dType))) (findResult state (ResultVariable (env state, name, dType)))
                let (ExprResult (varId, vdType)) = er
                id <- nextOpId
                let stackInst = [Asm.Inst (Asm.OpLoad id (searchTypeId state vdType) varId) Nothing]
                return (ExprResult (id, vdType), mempty, [], stackInst)

generateExprSt (Ast.EString _ _) = error "String"
generateExprSt (Ast.EUnit _) = error "Unit"
generateExprSt (Ast.EApp _ e1 e2) =
  do
    (r1, inst1, varInst1, stackInst1) <- generateExprSt e1
    (r2, inst2, varInst2, stackInst2) <- generateExprSt e2 >>= applyExpr

    let ExprApplication funcType (returnType, argTypes) args = r1
    let ExprResult var2 =  r2

    let args' = args ++ [var2]
    let r3 =ExprApplication funcType (returnType, argTypes) args'

    result <- if length argTypes == length args' then do
        -- if arg fullfilled then apply function
        (r4, inst3, varInst3, stackInst3) <- applyExpr (r3, inst1 +++ inst2, varInst1 ++ varInst2, stackInst1 ++ stackInst2)
        return (r4, inst3, varInst3, stackInst3)
      else do
        return (r3, inst1 +++ inst2, varInst1 ++ varInst2, stackInst1 ++ stackInst2)
    return result

generateExprSt (Ast.EIfThenElse (_,t) cond thenE elseE) =
  do
    let result =ExprApplication (IfElseApplication cond thenE elseE) (DT.DTypeUnknown, []) []
    return (result, mempty, [], [])
generateExprSt (Ast.ENeg _ e) =
  do
    (_er, inst1, varInst1, stackInst1) <- generateExprSt e
    let (ExprResult var) = _er
    (var', stackInst2) <- generateNegOpSt var
    return (ExprResult var', inst1, varInst1, stackInst1 ++ stackInst2)
generateExprSt (Ast.EBinOp _ e1 op e2) =
  do
    (_er, inst1, varInst1, stackInst1) <- generateExprSt e1 >>= applyExpr
    let ExprResult var1 = _er
    (_er, inst2, varInst2, stackInst2) <- generateExprSt e2 >>= applyExpr
    let ExprResult var2 = _er
    (var3, inst3, stackInst3) <- generateBinOpSt var1 op var2
    return (ExprResult var3, inst1 +++ inst2 +++ inst3, varInst1 ++ varInst2, stackInst1 ++ stackInst2 ++ stackInst3)
generateExprSt (Ast.EOp _ op) = do return (handleOp' op, mempty, [], [])
generateExprSt (Ast.ELetIn _ decs e) =
  do
    envs <- gets env
    modify (\s -> s{env = envs ++ [("letIn", DT.DTypeVoid)]})
    -- traceM (show decs)
    -- return $ error (show decs)
    let makeAssociative (a, b, c, d) = ([a], b, c, d)
    (_,inst, varInst, stackInst) <- foldMaplM (fmap makeAssociative . generateDecSt) decs
    (result, inst1, varInst2, stackInst1) <- generateExprSt e
    modify (\s -> s{env = envs})
    return (result, inst +++ inst1, varInst ++ varInst2, stackInst ++ stackInst1)

-- in error (show (findResult state2 (ResultVariableValue (env state2, "x", envType))))
-- in error (show (idMap state2))
-- in error (show decs)
