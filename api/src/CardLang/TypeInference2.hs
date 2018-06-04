{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CardLang.TypeInference2
  ( inferExpr
  )
  where

import Utils
import Types
import CardLang.Types

import qualified Data.Text as T
import qualified Data.HashMap.Strict  as M
import qualified Data.Set             as Set
import Control.Monad.RWS.Strict
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

data PType = Forall [Name] MType deriving (Show)
type WEnv = M.HashMap Name PType
type Substitution = M.HashMap Name MType

type InferState = [Name]
data InferError =
    CannotUnify MType MType
  | UnificationMismatch [MType] [MType]
  | InfiniteType Name MType
  | UnknownIdentifier Name
  deriving (Eq, Show)

type Constraint = (MType, MType)
type Infer a = (RWST
                  WEnv            -- Typing environment
                  [Constraint]    -- Generated constraints
                  InferState      -- Inference state
                  (Except         -- Inference errors
                    InferError)
                  a)              -- Result

-- SUBSTITUTION

class Substitutable a where
  apply :: Substitution -> a -> a
  ftv :: a -> Set.Set Name

instance Substitutable MType where
  apply _ (WConst a) = WConst a
  apply s t@(WVar a) = M.lookupDefault t a s
  apply s (WFun t1 t2) = WFun (apply s t1) (apply s t2)
  apply s x = error $ "Apply unimplementdY " <> show x
  
  ftv (WConst _) = mempty
  ftv (WVar a) = Set.singleton a

instance Substitutable Substitution where
  apply s1 s2 = M.map (apply s1) s2 `M.union` s1

instance Substitutable PType where
  ftv (Forall as t) = ftv t `Set.difference` (Set.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr ((<>) . ftv) mempty

instance Substitutable WEnv where
  ftv env = ftv $ M.elems env

-- UNIFICATION

uni :: MType -> MType -> Infer ()
uni t1 t2 = tell [(t1, t2)]

type Unifier = (Substitution, [Constraint])
type Solve a = StateT Unifier (ExceptT InferError Identity) a

-- TODO: Can unification create more constraints? Possibly, if we need to shunt
-- a constraint to the back because it can't be solved now?
unifies :: MType -> MType -> Solve Substitution
unifies t1 t2 | t1 == t2 = return mempty
unifies (WVar v) t = v `bind` t
unifies t (WVar v) = v `bind` t
unifies (WFun t1 t2) (WFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = error $ "Unifies unimplementdY " <> show t1 <> show t2

bind ::  Name -> MType -> Solve Substitution
bind a t | t == WVar a    = return mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return (M.singleton a t)

occursCheck ::  Substitutable a => Name -> a -> Bool
occursCheck name t = name `Set.member` ftv t

unifyMany :: [MType] -> [MType] -> Solve Substitution
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) =
  do s1 <- unifies t1 t2
     s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
     return $ s2 `apply` s1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

runSolver :: Solve Substitution -> MType -> [Constraint] -> Either InferError MType
runSolver m t cs = do
  (s, _) <- runIdentity $ runExceptT $ runStateT m (mempty, cs)

  return (apply s t)

solver :: Solve Substitution
solver = do
  (s0, cs) <- get

  case cs of
    [] -> return s0 -- No constraints left, we're done
    ((t1, t2):cs) -> do
      s1 <- unifies t1 t2
      put (s1 `apply` s0, cs)
      solver
-- HELPER FUNCTIONS
--
knownType = return . WConst

getEnv = ask

lookupEnv :: Name -> Infer MType
lookupEnv name = do
  env <- getEnv

  case M.lookup name env of
    Just x -> instantiate x
    Nothing -> throwError $ UnknownIdentifier name

inEnv :: (Name, PType) -> Infer a -> Infer a
inEnv (x, t) m = do
  let scope = M.insert x t
  local scope m

-- MAIN INFERNECE ROUTINE
infer :: UExpr -> Infer MType
infer = \case
  UConst (UInt _)      -> knownType "Int"
  UConst (UInt _)      -> knownType "Int"
  UConst (UString _)   -> knownType "String"
  UConst (UBool _)     -> knownType "Bool"
  UConst (ULocation _) -> knownType "Location"

  UVar x -> lookupEnv x

  -- A sequence is a heterogenous list of expressions - no unification is done
  -- between them. All expressions are evaluated, but only the final one is
  -- returned. As such, it is responsible for the type.
  USequence []     -> knownType "()"
  USequence [x]    -> infer x
  USequence (x:xs) -> do
    t1 <- infer x -- TODO: Pass forward env
    t2 <- infer $ USequence xs

    return t2
  
  UApp e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh

    uni t1 (t2 ~> tv)
    return tv

  ULet (name, value) expr -> do
    env <- getEnv
    t1 <- infer value
    t2 <- inEnv (name, generalize env t1) (infer expr)

    return t2

  UConst (UFunc _ name expr) -> do
    tv <- fresh
    t1 <- inEnv (name, Forall mempty tv) (infer expr)

    return $ tv ~> t1

  x -> error $ "Unknown expr: " <> show x

instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
  qs' <- mapM (const fresh) qs
  let s = M.fromList $ zip qs qs'
  return $ apply s t

generalize :: WEnv -> MType -> PType
generalize env mType = Forall qs mType
  where
    qs = toList $ ftv mType `Set.difference` ftv env

fresh :: Infer MType
fresh = do
  n:ns <- get
  put ns

  return . WVar $ n

--- Monads

runInfer :: UEnv -> Infer MType -> Either InferError (MType, [Constraint])
runInfer env m =
  let typeEnv = mempty in
  
  runExcept $ evalRWST m typeEnv nameSupply

nameSupply = supply
  where
    supply = fmap (T.singleton) ['a'..'z']

-- Public interface

inferExpr :: UEnv -> UExpr -> Either InferError MType
inferExpr env expr = case runInfer env (infer expr) of
  Left y -> Left y
  Right (t, cs) -> runSolver solver t cs

