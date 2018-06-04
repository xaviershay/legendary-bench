{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- !!!!!!!!!!!!!!!!!!!!!!! UNUSED !!!!!!!!!!!!!!
-- This is a WIP bit of code to adapt type inference to a constraint generation scheme outlined at https://kseo.github.io/posts/2017-01-02-hindley-milner-inference-with-constraints.html
-- The easy parts work (equality constraints), but I haven't yet implemented
-- implicit constraints that are necessary for let bindings to work.
--
-- It's trivial to change the tests to use this module instead, and the failing
-- tests show the work that remains.
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module CardLang.TypeInferenceConstraints
  ( inferExpr
  , mkTypeEnv
  , apply
  , compose
  )
  where

import Utils
import Types
import CardLang.Types

import Control.Lens
import qualified Data.Text as T
import qualified Data.HashMap.Strict  as M
import qualified Data.Set             as Set
import Data.List (nub)
import Control.Monad.RWS.Strict
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

data PType = Forall [Name] MType deriving (Show)
type WEnv = M.HashMap Name PType
type Substitution = M.HashMap Name MType

data InferState = InferState
  { _inferNames :: [Name]
  , _inferEnv :: WEnv
  }

makeLenses ''InferState

data InferError =
    CannotUnify MType MType
  | UnificationMismatch [MType] [MType]
  | InfiniteType Name MType
  | UnknownIdentifier Name
  deriving (Eq, Show)

type Constraint = (MType, MType)
type Infer a = (RWST
                  ()
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
  apply s (WList t) = WList $ apply s t
  apply s x = error $ "Apply unimplementdY " <> show x
  
  ftv (WConst _) = mempty
  ftv (WVar a) = Set.singleton a
  ftv (WFun t1 t2) = ftv t1 <> ftv t2
  ftv (WList t) = ftv t
  ftv (WBoardF t) = ftv t
  ftv x = error $ "ftv unimplemented" <> show x

--instance Substitutable Substitution where
--  apply s1 s2 = M.map (apply s1) s2 `M.union` s1

instance Substitutable PType where
  ftv (Forall as t) = ftv t `Set.difference` (Set.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr ((<>) . ftv) mempty

instance Substitutable a => Substitutable (a, a) where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 <> ftv t2

instance Substitutable WEnv where
  ftv env = ftv $ M.elems env

compose :: Substitution -> Substitution -> Substitution
compose a b = a `M.union` M.map (apply a) b

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
unifies (WList t1) (WList t2) = unifies t1 t2
unifies t1 t2 = throwError $ CannotUnify t1 t2

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
     return $ s2 `compose` s1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

runSolver :: Solve Substitution -> MType -> [Constraint] -> Either InferError MType
runSolver m t cs = do
  traceM "\n\n\n"
  traceM $ "Base type: " <> ppShow t
  traceM $ "Constraints: " <> ppShow cs
  (s, _) <- runIdentity $ runExceptT $ runStateT m (mempty, cs)
  traceM $ "S: " <> ppShow s

  return (apply s t)

solver :: Solve Substitution
solver = do
  (s0, cs) <- get

  case cs of
    [] -> return s0 -- No constraints left, we're done
    ((t1, t2):cs) -> do
      s1 <- unifies t1 t2
      put (s1 `compose` s0, apply s1 cs)
      solver
-- HELPER FUNCTIONS
--
knownType = return . WConst

getEnv = view inferEnv <$> get

lookupEnv :: Name -> Infer MType
lookupEnv name = do
  env <- getEnv

  case M.lookup name env of
    Just x -> instantiate x
    Nothing -> throwError $ UnknownIdentifier name

inEnv :: (Name, PType) -> Infer a -> Infer a
inEnv (x, t) m = do
  env <- getEnv

  modify (over inferEnv (M.insert x t))

  result <- m

  modify (set inferEnv env)

  return result

extendEnv :: (Name, PType) -> Infer ()
extendEnv (x, t) = do
  env <- getEnv

  traceM $ "Extending env with: " <> show x <> ", " <> show t

  modify (over inferEnv (M.insert x t))


-- MAIN INFERNECE ROUTINE
infer :: UExpr -> Infer MType
infer = \case
  UConst (UInt _)       -> knownType "Int"
  UConst (UInt _)       -> knownType "Int"
  UConst (UString _)    -> knownType "String"
  UConst (UBool _)      -> knownType "Bool"
  UConst (ULocation _)  -> knownType "Location"
  UConst (UList [])     -> WList <$> fresh
  UConst (UList (x:xs)) -> do
    t1 <- infer x
    t2 <- infer $ UConst (UList xs)

    uni (WList t1) t2

    return t2

  UDef name expr -> do
    env <- getEnv
    t1  <- infer expr

    traceM $ "\n\nUDef " <> show name <> ": " <> show t1
    traceM . show $ ftv t1
    traceM . show $ ftv env

    extendEnv (name, generalize env [] t1)


    return t1

  UVar x -> do
    env <- getEnv
    traceM $ "Lookup " <> show x <> ": " <> show (M.lookup x env)
    lookupEnv x

  -- A sequence is a heterogenous list of expressions - no unification is done
  -- between them. All expressions are evaluated, but only the final one is
  -- returned. As such, it is responsible for the type.
  USequence []     -> knownType "()"
  USequence [x]    -> infer x
  USequence (x:xs) -> do
    t1 <- infer x
    t2 <- infer $ USequence xs

    return t2
  
  UApp e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh

    traceM $ "Uapp new var: " <> show tv

    uni t1 (t2 ~> tv)
    return tv

  ULet (name, value) expr -> do
    traceM $ "ULet " <> show name
    env <- getEnv
    (t1, cs) <- listen (infer value)
    traceM $ "CS: " <> ppShow (ftv cs <> ftv env)
    t2 <- inEnv (name, generalize env cs t1) (infer expr)

    return t2

  UConst (UFunc _ name expr) -> do
    tv <- fresh
    t1 <- inEnv (name, Forall mempty tv) (infer expr)

    return $ tv ~> t1

  UIf cond lhs rhs -> do
    t1 <- infer cond
    t2 <- infer lhs
    t3 <- infer rhs

    uni t1 "Bool"
    uni t2 t3

    return t2

  x -> error $ "Unknown expr: " <> show x

instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
  traceM $ "Instantiating " <> show t
  qs' <- mapM (const fresh) qs
  let s = M.fromList $ zip qs qs'
  return $ apply s t

generalize :: WEnv -> [Constraint] -> MType -> PType
generalize env cs mType = Forall qs mType
  where
    qs = toList $ (ftv mType <> ftv cs) `Set.difference` ftv env

fresh :: Infer MType
fresh = do
  n:ns <- view inferNames <$> get
  traceM $ "-> New var " <> T.unpack n
  modify (set inferNames ns)

  return . WVar $ n

--- Monads

runInfer :: WEnv -> Infer MType -> Either InferError (MType, [Constraint])
runInfer typeEnv m =
  runExcept $ evalRWST m () (mkInferState typeEnv nameSupply)

mkInferState env supply = InferState
  { _inferNames = supply
  , _inferEnv   = env
  }

nameSupply = supply
  where
    supply = fmap (T.singleton) ['a'..'z']

normalize :: MType -> MType
normalize t = normalize' t
  where
    normalize' (WVar x) = WVar (fromJust $ M.lookup x names)
    normalize' (WList x) = WList (normalize' x)
    normalize' (WFun x y) = WFun (normalize' x) (normalize' y)
    normalize' x = x

    fv (WConst _) = []
    fv (WVar x) = [x]
    fv (WList x) = fv x
    fv (WFun x y) = fv x <> fv y

    names = M.fromList $ zip (nub . fv $ t) nameSupply

    fromJust Nothing = error "implementation error: type var not in names map"
    fromJust (Just x) = x

-- Public interface

inferExpr :: WEnv -> UExpr -> Either InferError MType
inferExpr env expr = trace (ppShow expr) $ case runInfer env (infer expr) of
  Left y -> Left y
  Right (t, cs) -> normalize <$> runSolver solver t cs

mkTypeEnv :: M.HashMap Name BuiltIn -> WEnv
mkTypeEnv defs = M.fromList
  [ ("reduce", Forall (["a", "b"]) (("b" ~> "a" ~> "b") ~> "b" ~> WList "a" ~> "b"))
  , ("concat", Forall (["a"]) (WList (WList "a") ~> WList "a"))
  ] <> M.map (Forall mempty . fst) defs
