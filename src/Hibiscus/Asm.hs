-- FIXME: WIP
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Hibiscus.Asm
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
-- import Text.Show.Deriving (deriveShow)


instance Emit Char where -- for String
  emit = show
instance Emit Int where
  emit = show
instance Emit (Int, OpId) where
  emit (x, y) = "(" ++ emit x ++ "," ++ emit y ++ ")"
instance (Emit a) => Emit [a] where
  emit = unwords . map emit -- join strings with space

data Literal
  = LBool Bool
  | LUint Int
  | LInt Int
  | LFloat Float
  deriving (Show, Eq, Ord)

instance Emit Literal where
  emit = show

data OpId
  = IdName String
  | Id Int
  deriving (Show)

instance Emit OpId where
  emit (IdName s) = "%" ++ s
  emit (Id i) = "%" ++ emit i

data Capability
  = Matrix
  | Shader
  deriving (Show)

instance Emit Capability where
  emit = show

data SourceLanguage
  = Unknown
  | HLSL
  | GLSL
  deriving (Show)

instance Emit SourceLanguage where
  emit = show

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

instance Emit StorageClass where
  emit = show

data ExecutionModel
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving (Show)

instance Emit ExecutionModel where
  emit = show

data ExecutionMode
  = Invocations Int
  | OriginUpperLeft
  | OriginLowerLeft
  deriving (Show)

instance Emit ExecutionMode where
  emit = show

data Decoration
  = RelaxedPrecision
  | SpecId Int
  | Block
  | Location Int
  deriving (Show)

instance Emit Decoration where
  emit RelaxedPrecision = "RelaxedPrecision"
  emit (SpecId i) = "SpecId " ++ show i
  emit Block = "Block"
  emit (Location i) = "Location " ++ show i

data FunctionControl
  = None
  | Inline
  | DontInline
  | Pure
  | Const
  deriving (Show)

instance Emit FunctionControl where
  emit = show

data AddressingModel
  = Logical
  | Physical32
  | Physical64
  deriving (Show)

instance Emit AddressingModel where
  emit = show

data MemoryModel
  = Simple
  | GLSL450
  | OpenCL
  | Vulkan
  deriving (Show)

instance Emit MemoryModel where
  emit = show

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
  | N_OpSourceContinued String
  | N_OpSource SourceLanguage Int
  | N_OpSourceExtension String
  | N_OpName OpId String
  | N_OpMemberName OpId Int String
  | N_OpString OpId String
  | N_OpLine Int Int
  | N_OpNoLine
  | -- OpAnnotation
    N_OpDecorate OpId Decoration
  | N_OpMemberDecorateStringGO OpId Int Decoration String
  | -- OpExtension
    N_OpExtension String
  | R_OpExtInstImport ResultId String
  | N_OpExtInst OpId OpId [OpId]
  | -- OpModeSetting
    N_OpMemoryModel AddressingModel MemoryModel
  | N_OpEntryPoint ExecutionModel OpId String [OpId]
  | N_OpExecutionMode OpId ExecutionMode
  | N_OpCapability Capability
  | -- OpTypeDeclaration
    R_OpTypeVoid ResultId
  | R_OpTypeBool ResultId
  | R_OpTypeInt ResultId Int Int -- bit width  , 0 indicates unsigned,1 indicates signed semantics.
  | R_OpTypeFloat ResultId Int -- bit width
  | R_OpTypeVector ResultId TypeId Int -- component count
  | R_OpTypeMatrix ResultId TypeId Int -- vectorTypeId column count
  | R_OpTypeArray ResultId TypeId ValueId -- data type id
  | R_OpTypeStruct ResultId [TypeId] -- data types id
  | R_OpTypePointer ResultId StorageClass TypeId
  | R_OpTypeFunction ResultId TypeId [TypeId] -- data types id
  | -- OpConstant
    R_OpConstantTrue ResultId ResultType
  | R_OpConstantFalse ResultId ResultType
  | R_OpConstant ResultId ResultType Literal
  | R_OpConstantComposite ResultId ResultType [OpId]
  | R_OpConstantSampler ResultId ResultType Int Int
  | R_OpConstantNull ResultId ResultType
  | -- OpMemory
    R_OpVariable ResultId TypeId StorageClass
  | R_OpLoad ResultId TypeId PointerId
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
    N_OpLabel LabelId
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
-- $(deriveShow ''Op)
instance Show Op where
  show = emit

type CommentStr = String

data Instruction = Inst Op (Maybe CommentStr)
                 | Comment CommentStr
  deriving (Show)

instance Emit Instruction where
  emit (Inst op maybeComment) = emit op ++ (maybe "" show maybeComment)
  emit (Comment s) = "; " ++ s

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
