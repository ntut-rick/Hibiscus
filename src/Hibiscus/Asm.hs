{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -w #-}

module Hibiscus.Asm
  ( Instruction (..),
    Literal (..),
    Ops (..),
    OpId (..),
    ShowList (..),
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
  )
where

data Literal
  = LBool Bool
  | LUint Int
  | LInt Int
  | LFloat Float
  deriving (Show, Eq, Ord)

data OpId
  = IdName String
  | Id Int

-- TODO: Improve type safety of OpId
type TypeId = OpId
type LabelId = OpId
type ValueId = OpId
type FunctionId = OpId
type PointerId = OpId

instance Show OpId where
  show (IdName s) = "%" ++ s
  show (Id i) = "%" ++ show i

type ResultType = OpId

newtype ShowList a = ShowList [a]

instance (Show a) => Show (ShowList a) where
  show (ShowList l) = unwords $ map show l -- join strings with space

type ResultId = OpId

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

data Ops
  = -- OpMiscellaneous
    OpNop
  | OpUndef ResultType
  | OpSourceContinued String
  | OpSource SourceLanguage Int
  | OpSourceExtension String
  | OpName OpId String
  | OpMemberName OpId Int String
  | OpString OpId String
  | OpLine Int Int
  | OpNoLine
  | -- OpAnnotation
    OpDecorate OpId Decoration
  | OpMemberDecorateStringGO OpId Int Decoration String
  | -- OpExtension
    OpExtension String
  | OpExtInstImport String
  | OpExtInst OpId OpId (ShowList OpId)
  | -- OpModeSetting
    OpMemoryModel AddressingModel MemoryModel
  | OpEntryPoint ExecutionModel OpId String (ShowList OpId)
  | OpExecutionMode OpId ExecutionMode
  | OpCapability Capability
  | -- OpTypeDeclaration
    OpTypeVoid
  | OpTypeBool
  | OpTypeInt Int Int -- bit width  , 0 indicates unsigned,1 indicates signed semantics.
  | OpTypeFloat Int -- bit width
  | OpTypeVector TypeId Int -- component count
  | OpTypeMatrix TypeId Int -- vectorTypeId column count
  | OpTypeArray TypeId ValueId -- data type id
  | OpTypeStruct (ShowList TypeId) -- data types id
  | OpTypePointer StorageClass TypeId
  | OpTypeFunction TypeId (ShowList TypeId) -- data types id
  | -- OpConstant
    OpConstantTrue ResultType
  | OpConstantFalse ResultType
  | OpConstant ResultType Literal
  | OpConstantComposite ResultType (ShowList OpId)
  | OpConstantSampler ResultType Int Int
  | OpConstantNull ResultType
  | -- OpMemory
    OpVariable TypeId StorageClass
  | OpLoad TypeId PointerId
  | OpStore PointerId ValueId
  | -- OpFunction
    OpFunction ResultType FunctionControl TypeId
  | OpFunctionParameter ResultType
  | OpFunctionEnd
  | OpFunctionCall ResultType FunctionId (ShowList PointerId)
  | -- OpConversion
    OpConvertFToU ResultType ValueId -- float to unsigned int
  | OpConvertFToS ResultType ValueId -- float to signed int
  | OpConvertSToF ResultType ValueId -- signed int to float
  | OpConvertUToF ResultType ValueId -- unsigned int to float
  | OpUConvert ResultType ValueId -- unsigned int to unsigned int
  | OpSConvert ResultType ValueId -- signed int to signed int
  | OpFConvert ResultType ValueId -- float to float
  | OpBitcast ResultType ValueId
  | -- OpComposite
    OpCompositeConstruct ResultType (ShowList ValueId)
  | OpCompositeExtract ResultType ValueId (ShowList Int)
  | OpCompositeInsert ResultType ValueId ValueId (ShowList ValueId)
  | -- OpArithmetic
    OpSNegate ResultType ValueId
  | OpFNegate ResultType ValueId
  | OpIAdd ResultType ValueId ValueId
  | OpISub ResultType ValueId ValueId
  | OpIMul ResultType ValueId ValueId
  | OpUDiv ResultType ValueId ValueId -- unsigned division
  | OpSDiv ResultType ValueId ValueId -- signed division
  | OpUMod ResultType ValueId ValueId -- unsigned modulo
  | OpSMod ResultType ValueId ValueId -- signed modulo
  | OpFAdd ResultType ValueId ValueId -- float
  | OpFSub ResultType ValueId ValueId
  | OpFMul ResultType ValueId ValueId
  | OpFDiv ResultType ValueId ValueId
  | OpFMod ResultType ValueId ValueId
  | OpFRem ResultType ValueId ValueId
  | OpVectorTimesScalar ResultType ValueId ValueId
  | OpVectorTimesMatrix ResultType ValueId ValueId
  | OpMatrixTimesScalar ResultType ValueId ValueId
  | OpMatrixTimesVector ResultType ValueId ValueId
  | OpMatrixTimesMatrix ResultType ValueId ValueId
  | -- OpLogical
    OpLogicalEqual ResultType ValueId ValueId
  | OpLogicalNotEqual ResultType ValueId ValueId
  | OpLogicalOr ResultType ValueId ValueId
  | OpLogicalAnd ResultType ValueId ValueId
  | OpLogicalNot ResultType ValueId
  | OpLogicalXor ResultType ValueId ValueId
  | OpIEqual ResultType ValueId ValueId -- int
  | OpINotEqual ResultType ValueId ValueId
  | OpUGreaterThan ResultType ValueId ValueId
  | OpSGreaterThan ResultType ValueId ValueId
  | OpUGreaterThanEqual ResultType ValueId ValueId
  | OpSGreaterThanEqual ResultType ValueId ValueId
  | OpULessThan ResultType ValueId ValueId
  | OpSLessThan ResultType ValueId ValueId
  | OpULessThanEqual ResultType ValueId ValueId
  | OpSLessThanEqual ResultType ValueId ValueId
  | OpFOrdEqual ResultType ValueId ValueId -- float
  | OpFUnordEqual ResultType ValueId ValueId
  | OpFOrdNotEqual ResultType ValueId ValueId
  | OpFUnordNotEqual ResultType ValueId ValueId
  | OpFOrdLessThan ResultType ValueId ValueId
  | OpFUnordLessThan ResultType ValueId ValueId
  | OpFOrdGreaterThan ResultType ValueId ValueId
  | OpFUnordGreaterThan ResultType ValueId ValueId
  | OpFOrdLessThanEqual ResultType ValueId ValueId
  | OpFUnordLessThanEqual ResultType ValueId ValueId
  | OpFOrdGreaterThanEqual ResultType ValueId ValueId
  | OpFUnordGreaterThanEqual ResultType ValueId ValueId
  | -- OpControlFlow
    OpLabel
  | OpBranch LabelId
  | OpBranchConditional ValueId LabelId LabelId
  | OpSelectionMerge LabelId FunctionControl
  | OpSwitch OpId OpId [(Int, OpId)]
  | OpKill
  | OpReturn
  | OpReturnValue ValueId
  | OpUnreachable
  | Comment String
  deriving (Show)

newtype Instruction = Instruction (Maybe ResultId, Ops)

instance Show Instruction where
  show (Instruction (_, Comment s)) = "; " ++ s
  show (Instruction (Just res, OpConstant r l)) =
    show res
      ++ " = "
      ++ "OpConstant "
      ++ show r
      ++ " "
      ++ ( case l of
             LBool b -> show b
             LUint i -> show i
             LInt i -> show i
             LFloat f -> show f
         )
  show (Instruction (Nothing, op)) = show op
  show (Instruction (Just res, op)) = show res ++ " = " ++ show op

test :: () -> [Instruction]
test () =
  [ Instruction (Nothing, OpTypeFloat 8),
    Instruction (Just (IdName "name"), OpTypeInt 32 0),
    Instruction (Just (Id 2), OpTypeInt 16 1)
  ]
