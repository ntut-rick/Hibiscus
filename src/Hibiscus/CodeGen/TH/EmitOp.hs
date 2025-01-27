{-# LANGUAGE TemplateHaskell #-}

module Hibiscus.CodeGen.TH.EmitOp where

import Control.Monad
import Control.Monad.Reader
import Language.Haskell.TH
import Hibiscus.CodeGen.Emit

-- Basically refered from http://web.archive.org/web/20100703060856/http://www.haskell.org/bz/thdoc.htm

type ConstructorNameStr = String
type FieldValueExp = Exp
type StrExp = Exp

type ConToStr = ConstructorNameStr -> [Q FieldValueExp] -> Q StrExp

ecAux :: Con -> ReaderT ConToStr Q Clause
ecAux (NormalC name fields) = do
  conToStr <- ask
  let
    genPE n =
      do
        ids <- replicateM n (newName "x")
        return (map varP ids, map varE ids)
  lift $ do
    -- Name of constructor, i.e. "A". Will become string literal in generated code
    let constructorName = nameBase name
    -- Get variables for left and right side of function definition
    (pats, vars) <- genPE (length fields)
    
    -- Generate function clause for one constructor
    let thePattern = [conP name pats] -- like: (A x1 x2)
    let theBody = normalB $ conToStr constructorName vars -- like: "A "++show x1++" "++show x2
    let anEmptyDecs = [] -- idk
    clause thePattern theBody anEmptyDecs

data DummyT = DummyT

deAux :: Name -> ReaderT ConToStr Q [InstanceDec]
deAux conName = do
  conToStr <- ask
  lift $ do
    TyConI (DataD _ _ _ _ constructors _) <- reify conName

    -- Make body:
    -- ex:
    --   emit (Op777 x1 x2) = show x1 ++ " = " ++ "Op777" ++ " " ++ show x2
    --   ...
    body <- runReaderT (mapM ecAux constructors) conToStr

    -- Generate template instance declaration
    d <- [d| instance Emit DummyT where emit _ = "dummy" |]
    let    [InstanceD mol [] (AppT emitt (ConT _      )) [FunD fname _   ]] = d
    -- and then replace type name (DummyT) and function body (\_ -> "dummy") with our data
    return [InstanceD mol [] (AppT emitt (ConT conName)) [FunD fname body]]

-- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
unwords'Q :: [Q Exp] -> Q Exp
unwords'Q []       = [| "" |]
unwords'Q (v:vars) = [| " " ++ show $v ++ $(unwords'Q vars) |]

mkEmit :: Con -> Q Clause
mkEmit (NormalC x_name fields) = runReaderT (ecAux (NormalC (mkName conName) fields)) conToStr
  where
    xnamestr@(prefix:_:conName) = nameBase x_name
    conToStr opName xs =
      case (prefix, xs) of 
        ('N', _)       -> [| opName ++ $(unwords'Q xs) |]
        ('R', fst:rst) -> [| show $fst ++ " = " ++ opName ++ $(unwords'Q rst) |]
        otherwise -> fail $ show xnamestr

-- Helper to modify constructor names
modifyConstructor :: (Name -> Name) -> Con -> Con
modifyConstructor modifyName (NormalC name fields) =
    NormalC (modifyName name) fields

removePrefix :: Name -> Name
removePrefix = mkName . drop 2 . nameBase

-- Example of how to replace constructor names in a data declaration
mkOpAndEmit :: Name -> Q [Dec]
mkOpAndEmit conName = do
    let newTypeName = removePrefix conName
    TyConI (DataD cxt _ tyVars kind cons derivs) <- reify conName

    -- Make body:
    -- ex:
    --   emit (Op777 x1 x2) = show x1 ++ " = " ++ "Op777" ++ " " ++ show x2
    --   ...
    body <- mapM mkEmit cons
    -- Generate template instance declaration
    _d <- [d| instance Emit DummyT where emit _ = "dummy" |]
    let               [InstanceD mol [] (AppT emitt (ConT _          )) [FunD fname _   ]] = _d
    -- and then replace type name (DummyT) and function body (\_ -> "dummy") with our data
    let instanceDec = [InstanceD mol [] (AppT emitt (ConT newTypeName)) [FunD fname body]]

    -- Modify the constructor names
    let updatedCons = map (modifyConstructor removePrefix) cons
    -- Create a new data type with the new name
    let newData = [DataD cxt newTypeName tyVars kind updatedCons derivs]
    return $ newData ++ instanceDec


mkEmitOp :: Name -> Q [InstanceDec]
mkEmitOp = undefined
