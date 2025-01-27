module Hibiscus.CodeGen.Emit where

class Emit a where
  emit :: a -> String
