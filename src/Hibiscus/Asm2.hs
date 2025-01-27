-- FIXME: WIP
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Hibiscus.Asm2
  ( Instruction (..),
    Literal (..),
    Op (..),
    OpId (..),
    Capability (..),
    AddressingModel (..),
    SourceLanguage (..),
    StorageClass (..),
    Decoration (..),
    FunctionControl (..),
    MemoryModel (..),
    ExecutionModel (..),
    ExecutionMode (..),
    ResultId,
    Emit (..),
    )
  where

import Hibiscus.CodeGen.Emit (Emit, emit)
import Hibiscus.CodeGen.TH.EmitOp (mkOpAndEmit)

data Literal
  = LBool Bool
  | LUint Int
  | LInt Int
  | LFloat Float
  deriving (Show, Eq, Ord)

data OpId
  = IdName String
  | Id Int

instance Show OpId where
  show (IdName s) = "%" ++ s
  show (Id i) = "%" ++ show i

data Capability
  = Matrix
  | Shader
  deriving (Show)

data SourceLanguage
  = Unknown
  | HLSL
  | GLSL
  deriving (Show)

data StorageClass
  = Function
  | Input
  | Output
  | UniformConstant
  | Uniform
  | Private
  | FunctionParameter
  | Incoming
  | Outgoing
  | Pointer
  deriving (Show, Eq, Ord)

data ExecutionModel
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving (Show)

data ExecutionMode
  = Invocations Int
  | OriginUpperLeft
  | OriginLowerLeft
  deriving (Show)

data Decoration
  = RelaxedPrecision
  | SpecId Int
  | Block
  | Location Int

instance Show Decoration where
  show RelaxedPrecision = "RelaxedPrecision"
  show (SpecId i) = "SpecId " ++ show i
  show Block = "Block"
  show (Location i) = "Location " ++ show i

data FunctionControl
  = None
  | Inline
  | DontInline
  | Pure
  | Const
  deriving (Show)

data AddressingModel
  = Logical
  | Physical32
  | Physical64
  deriving (Show)

data MemoryModel
  = Simple
  | GLSL450
  | OpenCL
  | Vulkan
  deriving (Show)

type ResultId = OpId
type ResultType = OpId

type TypeId = OpId
type LabelId = OpId
type ValueId = OpId
type FunctionId = OpId
type PointerId = OpId

-- FIXME: Returned Op
-- NOTE: X_ is for Template Haskell
--         T_ just to avoid multipled define
--         N_ means no returned
--         R_ means returned.
--       will be removed by TH
data T_Op
  = -- OpMiscellaneous
    N_OpNop
  | R_OpUndef ResultId ResultType
  | R_OpSourceContinued String
  | R_OpSource SourceLanguage Int
  | R_OpSourceExtension String
  | R_OpName OpId String
  | R_OpMemberName OpId Int String
  | R_OpString OpId String
  | R_OpLine Int Int
  | N_OpNoLine
  | -- OpAnnotation
    R_OpDecorate OpId Decoration
  | R_OpMemberDecorateStringGO OpId Int Decoration String
  | -- OpExtension
    N_OpExtension String
  | N_OpExtInstImport String
  | N_OpExtInst OpId OpId [OpId]
  | -- OpModeSetting
    R_OpMemoryModel AddressingModel MemoryModel
  | R_OpEntryPoint ExecutionModel OpId String [OpId]
  | R_OpExecutionMode OpId ExecutionMode
  | R_OpCapability Capability
  | -- OpTypeDeclaration
    N_OpTypeVoid
  | N_OpTypeBool
  | R_OpTypeInt Int Int -- bit width  , 0 indicates unsigned,1 indicates signed semantics.
  | R_OpTypeFloat Int -- bit width
  | R_OpTypeVector TypeId Int -- component count
  | R_OpTypeMatrix TypeId Int -- vectorTypeId column count
  | R_OpTypeArray TypeId ValueId -- data type id
  | R_OpTypeStruct [TypeId] -- data types id
  | R_OpTypePointer StorageClass TypeId
  | R_OpTypeFunction TypeId [TypeId] -- data types id
  | -- OpConstant
    R_OpConstantTrue ResultId ResultType
  | R_OpConstantFalse ResultId ResultType
  | R_OpConstant ResultId ResultType Literal
  | R_OpConstantComposite ResultId ResultType [OpId]
  | R_OpConstantSampler ResultId ResultType Int Int
  | R_OpConstantNull ResultId ResultType
  | -- OpMemory
    R_OpVariable TypeId StorageClass
  | R_OpLoad TypeId PointerId
  | N_OpStore PointerId ValueId
  | -- OpFunction
    R_OpFunction ResultId ResultType FunctionControl TypeId
  | R_OpFunctionParameter ResultId ResultType
  | N_OpFunctionEnd
  | R_OpFunctionCall ResultId ResultType FunctionId [PointerId]
  | -- OpConversion
    R_OpConvertFToU ResultId ResultType ValueId -- float to unsigned int
  | R_OpConvertFToS ResultId ResultType ValueId -- float to signed int
  | R_OpConvertSToF ResultId ResultType ValueId -- signed int to float
  | R_OpConvertUToF ResultId ResultType ValueId -- unsigned int to float
  | R_OpUConvert ResultId ResultType ValueId -- unsigned int to unsigned int
  | R_OpSConvert ResultId ResultType ValueId -- signed int to signed int
  | R_OpFConvert ResultId ResultType ValueId -- float to float
  | R_OpBitcast ResultId ResultType ValueId
  | -- OpComposite
    R_OpCompositeConstruct ResultId ResultType [ValueId]
  | R_OpCompositeExtract ResultId ResultType ValueId [Int]
  | R_OpCompositeInsert ResultId ResultType ValueId ValueId [ValueId]
  | -- OpArithmetic
    R_OpSNegate ResultId ResultType ValueId
  | R_OpFNegate ResultId ResultType ValueId
  | R_OpIAdd ResultId ResultType ValueId ValueId
  | R_OpISub ResultId ResultType ValueId ValueId
  | R_OpIMul ResultId ResultType ValueId ValueId
  | R_OpUDiv ResultId ResultType ValueId ValueId -- unsigned division
  | R_OpSDiv ResultId ResultType ValueId ValueId -- signed division
  | R_OpUMod ResultId ResultType ValueId ValueId -- unsigned modulo
  | R_OpSMod ResultId ResultType ValueId ValueId -- signed modulo
  | R_OpFAdd ResultId ResultType ValueId ValueId -- float
  | R_OpFSub ResultId ResultType ValueId ValueId
  | R_OpFMul ResultId ResultType ValueId ValueId
  | R_OpFDiv ResultId ResultType ValueId ValueId
  | R_OpFMod ResultId ResultType ValueId ValueId
  | R_OpFRem ResultId ResultType ValueId ValueId
  | R_OpVectorTimesScalar ResultId ResultType ValueId ValueId
  | R_OpVectorTimesMatrix ResultId ResultType ValueId ValueId
  | R_OpMatrixTimesScalar ResultId ResultType ValueId ValueId
  | R_OpMatrixTimesVector ResultId ResultType ValueId ValueId
  | R_OpMatrixTimesMatrix ResultId ResultType ValueId ValueId
  | -- OpLogical
    R_OpLogicalEqual ResultId ResultType ValueId ValueId
  | R_OpLogicalNotEqual ResultId ResultType ValueId ValueId
  | R_OpLogicalOr ResultId ResultType ValueId ValueId
  | R_OpLogicalAnd ResultId ResultType ValueId ValueId
  | R_OpLogicalNot ResultId ResultType ValueId
  | R_OpLogicalXor ResultId ResultType ValueId ValueId
  | R_OpIEqual ResultId ResultType ValueId ValueId -- int
  | R_OpINotEqual ResultId ResultType ValueId ValueId
  | R_OpUGreaterThan ResultId ResultType ValueId ValueId
  | R_OpSGreaterThan ResultId ResultType ValueId ValueId
  | R_OpUGreaterThanEqual ResultId ResultType ValueId ValueId
  | R_OpSGreaterThanEqual ResultId ResultType ValueId ValueId
  | R_OpULessThan ResultId ResultType ValueId ValueId
  | R_OpSLessThan ResultId ResultType ValueId ValueId
  | R_OpULessThanEqual ResultId ResultType ValueId ValueId
  | R_OpSLessThanEqual ResultId ResultType ValueId ValueId
  | R_OpFOrdEqual ResultId ResultType ValueId ValueId -- float
  | R_OpFUnordEqual ResultId ResultType ValueId ValueId
  | R_OpFOrdNotEqual ResultId ResultType ValueId ValueId
  | R_OpFUnordNotEqual ResultId ResultType ValueId ValueId
  | R_OpFOrdLessThan ResultId ResultType ValueId ValueId
  | R_OpFUnordLessThan ResultId ResultType ValueId ValueId
  | R_OpFOrdGreaterThan ResultId ResultType ValueId ValueId
  | R_OpFUnordGreaterThan ResultId ResultType ValueId ValueId
  | R_OpFOrdLessThanEqual ResultId ResultType ValueId ValueId
  | R_OpFUnordLessThanEqual ResultId ResultType ValueId ValueId
  | R_OpFOrdGreaterThanEqual ResultId ResultType ValueId ValueId
  | R_OpFUnordGreaterThanEqual ResultId ResultType ValueId ValueId
  | -- OpControlFlow
    N_OpLabel
  | N_OpBranch LabelId
  | N_OpBranchConditional ValueId LabelId LabelId
  | N_OpSelectionMerge LabelId FunctionControl
  | N_OpSwitch OpId OpId [(Int, OpId)]
  | N_OpKill
  | N_OpReturn
  | N_OpReturnValue ValueId
  | N_OpUnreachable
  deriving (Show)

$(mkOpAndEmit ''T_Op)

type CommentStr = String

data Instruction = Inst Op (Maybe CommentStr)
                 | Comment CommentStr

-- data FunctionInst = FunctionInst
--   { begin :: [Asm.Instruction]
--   , parameter :: [Asm.Instruction] 
--    --  %4 = OpFunction %2 None %3                  ; main()
--   , label :: [Asm.Instruction]
--    --  %5 = OpLabel
--   , variable :: [Asm.Instruction]
--    --  %9 = OpVariable %8 Function
--    -- %48 = OpVariable %47 Function
--   , body :: [Asm.Instruction]
--    -- ...
--   --, end :: [Asm.Instruction] -- move to printer
--    --       OpFunctionEnd
--   }
--   deriving (Show)

-- toInstruction :: FunctionInst -> [FunctionInst]
-- toInstruction (FunctionInst header param )

-- data WholeTheFile = WTF
--   { headerFields :: HeaderFields -- HACK: Maybe
--   , nameFields :: [Instruction]
--   , uniformsFields :: [Instruction] -- Annote I/O
--   , typeFields :: [Instruction] -- declare type
--   , functionFields :: [FunctionInst] -- All functions
--   }
--   deriving (Show)

-- class Emit a where
--   emit :: a -> String

-- instance Emit OpId where
--   emit (IdName s) = "%" ++ s
--   emit (Id i) = "%" ++ show i

-- instance Emit Literal where
--   emit lit = 
--     case lit of
--       LBool b -> show b
--       LUint i -> show i
--       LInt i -> show i
--       LFloat f -> show f

-- instance Emit Instruction where
--   emit Comment = "; " ++ s
--   emit (InstA (Just resultId, OpConstant typeId lit)) =
--     show resultId ++ " = OpConstant " ++ show typeId ++ " " ++ emit lit
--   emit (Instruction (Nothing, op)) = show op
--   emit (Instruction (Just resultId, op)) = show resultId ++ " = " ++ show op

-- type MaybeComment = Maybe String
