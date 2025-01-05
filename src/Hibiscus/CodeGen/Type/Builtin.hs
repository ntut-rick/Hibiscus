module Hibiscus.CodeGen.Type.Builtin where

import Hibiscus.CodeGen.Type.DataType
import Hibiscus.CodeGen.Types

getBulitinFunctionType :: String -> Maybe BaseFunctionType
getBulitinFunctionType name = 
  case name of
    "Int" -> return $ FTConstructor int32
    "Float" -> return $ FTConstructor float32
    "Bool" -> return $ FTConstructor bool
    "Vec2" -> return $ FTConstructor vector2
    "Vec3" -> return $ FTConstructor vector3
    "Vec4" -> return $ FTConstructor vector4
    "foldl" -> return $ FTFoldl
    "map" -> return $ FTMap
    "index_float" -> return $ FTIndex float32
    "extract_0" -> return $ FTExtractor float32 [0]
    "extract_1" -> return $ FTExtractor float32 [1]
    "extract_2" -> return $ FTExtractor float32 [2]
    "extract_3" -> return $ FTExtractor float32 [3]
    _ -> Nothing

getBuiltinType :: String -> Maybe DataType
getBuiltinType t = case t of
  "Int" -> Just int32
  "Float" -> Just float32
  "Bool" -> Just bool
  "Vec2" -> Just vector2
  "Vec3" -> Just vector3
  "Vec4" -> Just vector4
  -- "int" -> Just int32
  -- "float" -> Just float32
  -- "bool" -> Just bool
  -- "vec2" -> Just vector2
  -- "vec3" -> Just vector3
  -- "vec4" -> Just vector4
  _ -> Nothing
