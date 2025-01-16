{-# LANGUAGE TemplateHaskell #-}

module Hibiscus.CodeGen.TH.EmitOp where

import Control.Monad
import Language.Haskell.TH

class Emit a where
  emit :: a -> String

-- Basically refered from http://web.archive.org/web/20100703060856/http://www.haskell.org/bz/thdoc.htm

type ConstructorNameStr = String
type FieldValueExp = Exp
type StrExp = Exp

ecAux :: (ConstructorNameStr -> [Q FieldValueExp] -> Q StrExp) -> Con -> Q Clause
ecAux conToStr (NormalC name fields) =
  let
    genPE n =
      do
        ids <- replicateM n (newName "x")
        return (map varP ids, map varE ids)
  in do
    -- Name of constructor, i.e. "A". Will become string literal in generated code
    let constructorName = nameBase name
    -- Get variables for left and right side of function definition
    (pats,vars) <- genPE (length fields)

    -- Generate function clause for one constructor
    let thePattern = [conP name pats] -- like: (A x1 x2)
    let thebody = normalB $ conToStr constructorName vars -- like: "A "++show x1++" "++show x2
    let anEmptyDecs = [] -- idk
    clause thePattern thebody anEmptyDecs

data DummyT = DummyT

deAux :: (ConstructorNameStr -> [Q FieldValueExp] -> Q StrExp) -> Name -> Q [InstanceDec]
deAux conToStr conName = do
  TyConI (DataD _ _ _ _ constructors _)  <- reify conName

  -- Make body:
  -- ex:
  --   emit (Op777 x1 x2) = show x1 ++ " = " ++ "Op777" ++ " " ++ show x2
  --   ...
  body <- mapM (ecAux conToStr) constructors

  -- Generate template instance declaration
  d <- [d| instance Emit DummyT where emit _ = "dummy" |]
  let    [InstanceD mol [] (AppT emitt (ConT _      )) [FunD fname _   ]] = d
  -- and then replace type name (DummyT) and function body (\_ -> "dummy") with our data
  return [InstanceD mol [] (AppT emitt (ConT conName)) [FunD fname body]]

-- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
unwords'Q :: [Q Exp] -> Q Exp
unwords'Q []       = [| "" |]
unwords'Q (v:vars) = [| " " ++ show $v ++ $(unwords'Q vars) |]

deriveEmitForNoreturnedOp :: Name -> Q [InstanceDec]
deriveEmitForNoreturnedOp = deAux conToStr
  where
    conToStr opName xs = [| opName ++ $(unwords'Q xs) |]

deriveEmitForReturnedOp :: Name -> Q [InstanceDec]
deriveEmitForReturnedOp = deAux conToStr
  where
    conToStr opName (fst:rst) = [| show $fst ++ " = " ++ opName ++ $(unwords'Q rst) |]
