{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hibiscus.TypeInfer.Type4plus where

import Hibiscus.Ast
import Hibiscus.Util (fmap2nd, foldMapM, foldMaprM)

import Data.Bifunctor
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldlM, foldrM)
import Data.Functor (void)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Tuple (curry)
import Prelude hiding (lookup)

import Debug.Trace
import GHC.Stack (HasCallStack)

import Hibiscus.TypeInfer.RSF

type TypeEnv = Map.Map (Name ()) (Type ())
newtype Subst = Subst (Map.Map MetaSymbol (Type ())) deriving (Show) -- responsible to maintain all metas

instance Semigroup Subst where
  s1@(Subst map1) <> (Subst map2) = Subst $ Map.map (applySub s1) map2 `Map.union` map1
instance Monoid Subst where
  mempty = Subst mempty
lookup :: (Name a) -> TypeEnv -> Maybe (Type ())
lookup n env = Map.lookup (void n) env

class Substable a where
  applySub :: Subst -> a -> a
instance Substable (Type ()) where
  applySub (Subst sub) t@(TUnknown _ n) =
    -- trace ("applySub: " ++ show sub) $
    fromMaybe t (Map.lookup n sub)
  applySub s (TPar _ t) = applySub s t
  applySub s (TArrow _ ta tb) = TArrow () ta' tb'
   where
    ta' = applySub s ta
    tb' = applySub s tb
  applySub _ t = t

literalT :: String -> Type ()
literalT = TVar () . Name () . pack

unifyRS :: Type () -> Type () -> RSF e Subst ()
unifyRS t1_ t2_ =
  let
    bindVar :: MetaSymbol -> Type () -> RSF e Subst ()
    bindVar v t = do
      let newSub = Subst $ Map.fromList [(v, t)]
      modify (\s -> newSub <> s)
   in
    do
      s <- get
      let t1 = applySub s t1_
      let t2 = applySub s t2_
      -- traceM ("unifying: " ++ show t1 ++ " ==? " ++ show t2)
      case (t1, t2) of
        _ | t1 == t2 -> return ()
        (TPar _ t1, _) -> unifyRS t1 t2
        (_, TPar _ t2) -> unifyRS t1 t2
        (TList _ t1, TList _ t2) -> unifyRS t1 t2
        (TArray _ l1 t1, TArray _ l2 t2) | l1 == l2 -> unifyRS t1 t2
        (TArray _ _ t1, TList _ t2) -> unifyRS t1 t2
        (TList _ t1, TArray _ _ t2) -> unifyRS t1 t2
        (TUnknown _ v, t) -> bindVar v t
        (t, TUnknown _ v) -> bindVar v t
        (TArrow _ t1 t2, TArrow _ t1' t2') -> do
          unifyRS t1 t1'
          unifyRS t2 t2'
        _ -> error $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

freshTypeUnkRS :: RSF TypeEnv Subst (Type ())
freshTypeUnkRS =
  do
    lastnum <- gets (\(Subst s) -> maximum $ [0] ++ Map.keys s)
    let newSym = 1 + lastnum

    let t' = TUnknown () newSym
    let nm = Subst $ Map.fromList [(newSym, t')]
    modify (\s -> nm <> s)
    return t'

withDecsRS :: [Dec a] -> RSF TypeEnv Subst b -> RSF TypeEnv Subst b
withDecsRS decs = withRSF' (envFromRS' $ fmap void decs)

envFromRS' :: [Dec ()] -> TypeEnv -> Subst -> Result (TypeEnv, Subst)
envFromRS' decs r s = foldrM decToRS (r, s) decs

decToRS :: Dec () -> (TypeEnv, Subst) -> Result (TypeEnv, Subst)
decToRS (DecAnno _ n t) (env, s) =
  case lookup n env of
    Just t' ->
      case execRSF (unifyRS t t') env s of
        Right s' -> return (env, s')
        Left x -> fail x
    Nothing -> do
      let env' = Map.fromList [(n, t)]
      return (env' <> env, s)
decToRS (Dec _ n _ _) (env, s) =
  case lookup n env of
    Just t ->
      if isUnknown t
        then fail $ "duplicated defined " ++ show n
        else return (env, s)
    Nothing -> do
      let Right (t, s') = runRSF freshTypeUnkRS env s
      let env' = Map.fromList [(n, t)]
      return (env' <> env, s')

addType :: (Functor f) => Type b -> f a -> f (a, Type b)
addType t = fmap (\a -> (a, t))
getType :: (Foldable f, Functor f) => f (a, Type b) -> Type b
getType = snd . foldr1 (\aa _ -> aa) -- XXX: IDK what exectly foldr1 do
forget :: (Functor f) => f (a, Type b) -> f a
forget = fmap fst

applySubM :: (Functor f) => Subst -> f (a, Type ()) -> f (a, Type ())
applySubM sub = fmap2nd (applySub sub)

argToEnv :: [Argument (a, Type ())] -> TypeEnv
argToEnv = Map.fromList . map (\(Argument (_, t) n) -> (void n, t))

inferExprRS :: Expr a -> RSF TypeEnv Subst (Expr (a, Type ()))
inferExprRS e@(EInt _ _) = return $ addType (literalT "Int") e
inferExprRS e@(EFloat _ _) = return $ addType (literalT "Float") e
inferExprRS e@(EString _ _) = return $ addType (literalT "String") e
inferExprRS e@(EBool _ _) = return $ addType (literalT "Bool") e
inferExprRS e@(EUnit _) = return $ addType (literalT "Unit") e
inferExprRS (EPar _ e) = inferExprRS e
inferExprRS e@(EVar _ x) =
  do
    env <- ask
    case lookup x env of
      Nothing -> fail $ "Unbound variable: " ++ show x
      Just t -> return $ addType t e
inferExprRS (EList a exprs) =
  do
    exprs' <- foldrM aux [] exprs
    t <- maybe freshTypeUnkRS (return . getType) $ listToMaybe exprs'
    return $ EList (a, TArray () (length exprs') t) exprs'
 where
  -- aux :: Expr a -> (Subst, [Expr (a, Type ())]) -> Result (Subst, [Expr (a, Type ())])
  aux :: Expr a -> [Expr (a, Type ())] -> RSF TypeEnv Subst [Expr (a, Type ())]
  aux expr acc =
    do
      expr' <- inferExprRS expr
      s20 <- get
      -- check if type same as previous
      case acc of
        (x : _) -> unifyRS (getType x) (getType expr')
        [] -> modify id
      finalSub <- get
      return $ fmap (applySubM finalSub) (expr' : acc)
inferExprRS (ELetIn a decs body) =
  do
    (decs', body') <- withDecsRS decs miniworld
    return $ ELetIn (a, getType body') decs' body'
 where
  miniworld =
    do
      decs' <- inferDecsRS decs
      body' <- inferExprRS body
      return (decs', body')
inferExprRS (EApp a f x) =
  do
    f' <- inferExprRS f
    x' <- inferExprRS x
    let tf = getType f'
    let tx = getType x'
    tv <- freshTypeUnkRS
    unifyRS tf (TArrow () tx tv)
    return $ EApp (a, tv) f' x'
inferExprRS (ENeg a e) =
  do
    e' <- inferExprRS e
    return $ ENeg (a, getType e') e'
inferExprRS e@(EOp _ op) =
  do
    xt <- freshTypeUnkRS
    let ft = TArrow () xt xt
    return $ addType ft e
inferExprRS (EBinOp a e1 biop e2) =
  do
    e1' <- inferExprRS e1
    e2' <- inferExprRS e2
    let t1 = getType e1'
    let t2 = getType e2'
    unifyRS t1 t2
    let eType = t1
    let finalType = if isBooleanOp biop then literalT "Bool" else eType
    return $ EBinOp (a, finalType) e1' (addType finalType biop) e2'
inferExprRS (EIfThenElse a condE thenE elseE) =
  do
    condE' <- inferExprRS condE
    elseE' <- inferExprRS elseE
    thenE' <- inferExprRS thenE
    let
    unifyRS (getType condE') (literalT "Bool")
    unifyRS (getType elseE') (getType thenE')
    let finalType = getType elseE'
    return $ EIfThenElse (a, finalType) condE' thenE' elseE'
-- TODO: fold
inferExprRS expr =
  do
    traceM $ "[WARN] Not implement Expr: " ++ show (void expr)
    t <- freshTypeUnkRS
    return $ addType t expr

inferDecsRS :: [Dec a] -> RSF TypeEnv Subst [Dec (a, Type ())]
inferDecsRS = mapRSF (\(ds, s) -> (fmap (applySubM s) ds, s)) . foldMaprM aux
 where
  aux :: Dec a -> RSF TypeEnv Subst [Dec (a, Type ())]
  aux (Dec a name args body) =
    do
      bodyType <- freshTypeUnkRS

      argWithTypes <- foldMapM magic args
      let funcType = foldr (TArrow ()) bodyType (map getType argWithTypes)
      let innerEnv = argToEnv argWithTypes
      body' <- withRSF (\r s -> (innerEnv <> r, s)) (inferExprRS body)
      unifyRS (getType body') bodyType

      prefinalType <- asks (fromJust . lookup name)
      unifyRS prefinalType funcType

      let finalName = addType (TUnit ()) name
      let finalDec = Dec (a, funcType) finalName argWithTypes body'
      currSub <- get
      return [finalDec]
  aux microencourage = return []
  magic :: Argument a -> RSF TypeEnv Subst ([Argument (a, Type ())])
  magic arg =
    do
      t <- freshTypeUnkRS
      return $ [addType t arg]

inferDecs :: (TypeEnv, Subst) -> [Dec a] -> Result [Dec (a, Type ())]
inferDecs (env, sub) decs = evalRSF (inferDecsRS decs) env sub

infer :: [Dec a] -> Result [Dec (a, Type ())]
infer decs = evalRSF (withDecsRS decs (inferDecsRS decs)) mempty mempty
