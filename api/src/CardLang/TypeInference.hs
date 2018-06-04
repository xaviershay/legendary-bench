{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module CardLang.TypeInference
  ( typecheck
  , showType
  , InferError(..)
  ) where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State  (State, evalState, get, lift, put)
import qualified Data.HashMap.Strict  as M
import           Data.List            (nub)
import           Data.Maybe           (fromJust)
import qualified Data.Set             as Set
import qualified Data.Text            as T

import CardLang.Types
import CardLang.Evaluator
import Utils

showType (WVar x) = x
showType (WConst x) = x
showType (WFun x y) = maybeBracket (isFun x) (showType x) <> " -> " <> showType y
  where
    isFun (WFun{}) = True
    isFun _ = False
    maybeBracket cond x = if cond then "(" <> x <> ")" else x
showType (WList x) = "[" <> showType x <> "]"
showType (WBoardF x) = "@:" <> showType x

newtype Subst = Subst (M.HashMap Name MType) deriving (Show)

instance Monoid Subst where
  mempty = Subst M.empty
  mappend s1@(Subst a) s2@(Subst b) = Subst (a `M.union` b')
    where
      Subst b' = applySubst s1 s2

instance Monoid WEnv where
  mempty = WEnv mempty
  mappend (WEnv a) (WEnv b) = WEnv (a <> b)

class Substitutable a where
  applySubst :: Subst -> a -> a

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  applySubst s (x, y) = (applySubst s x, applySubst s y)

instance Substitutable Subst where
  applySubst s (Subst target) = Subst (fmap (applySubst s) target)

instance Substitutable MType where
  applySubst (Subst s) c@(WVar a) = M.lookupDefault c a s
  applySubst s c@(WConst {}) = c
  applySubst s (WFun f x) = WFun (applySubst s f) (applySubst s x)
  applySubst s (WBoardF x) = WBoardF (applySubst s x)
  applySubst s (WList x) = WList (applySubst s x)
  applySubst a b = error $ "applySubst error " <> show a <> ", " <> show b

freeMType :: MType -> Set.Set Name
freeMType (WBoardF x) = freeMType x
freeMType (WList x) = freeMType x
freeMType (WConst _) = mempty
freeMType (WVar x) = Set.singleton x
freeMType (WFun x y) = freeMType x <> freeMType y

data PType = Forall (Set.Set Name) MType deriving (Show)

freePType :: PType -> Set.Set Name
freePType (Forall qs mType) = freeMType mType `Set.difference` qs

instance Substitutable PType where
  -- Remove the free types from the env and then substitute mType with that
  applySubst (Subst s) (Forall qs mType) =
    let qs' = M.fromList (fmap (\t -> (t, ())) $ toList qs)
        s'  = Subst (s `M.difference` qs')
    in Forall qs (applySubst s' mType)

newtype WEnv = WEnv (M.HashMap Name PType) deriving (Show)

freeEnv :: WEnv -> Set.Set Name
freeEnv (WEnv e) = Set.unions (fmap freePType $ M.elems e)

instance Substitutable WEnv where
  applySubst s (WEnv env) = WEnv (M.map (applySubst s) env)

newtype Infer a = Infer (ExceptT InferError (State [Name]) a)
  deriving (Functor, Applicative, Monad)

data InferError =
    CannotUnify MType MType
  | OccursCheckFailed Name MType
  | UnknownIdentifier Name
  deriving (Eq)

instance Show InferError where
  show (CannotUnify a b) = T.unpack $ "Cannot unify:\n  " <> showType a <> "\n  " <> showType b
  show (UnknownIdentifier n) = "Unknown identifier: " <> T.unpack n

typecheck :: UExpr -> Either InferError MType
typecheck expr = recode . snd <$> runInfer (infer builtInTypeEnv expr)

builtInTypeEnv :: WEnv
builtInTypeEnv = WEnv $ M.fromList
  [ ("reduce", Forall (Set.fromList ["a", "b"]) (("b" ~> "a" ~> "b") ~> "b" ~> WList "a" ~> "b"))
  , ("concat", Forall (Set.fromList ["a"]) (WList (WList "a") ~> WList "a"))
  ] <> M.map (Forall mempty . fst) builtIns

boardFuncs = mempty
--boardFuncs = M.fromList
--  [ ("current-player", "PlayerId")
--  , ("player-location", "PlayerId" ~> "ScopedLocation" ~> "Location")
--  , ("card-at", "SpecificLocation" ~> "Card")
--  , ("cards-at", "Location" ~> WList "Card")
--  ]

boardFuncTypeEnv :: WEnv
boardFuncTypeEnv = WEnv $ M.map (Forall mempty) boardFuncs

runInfer :: Infer a -> Either InferError a
runInfer (Infer inf) = evalState (runExceptT inf) typeNames

alphabet = map T.singleton ['a'..'z']
typeNames = (infiniteSupply alphabet)
-- [a, b, c] ==> [a,b,c, a1,b1,c1, a2,b2,c2, …]
infiniteSupply supply = supply <> addSuffixes supply (1 :: Integer)
  where
    addSuffixes xs n = map (\x -> addSuffix x n) xs <> addSuffixes xs (n+1)

throw :: InferError -> Infer a
throw = Infer . throwError

bindVariableTo :: Name -> MType -> Infer Subst
bindVariableTo name (WVar v) | name == v = return mempty
bindVariableTo name mType | name `occursIn` mType = throw (OccursCheckFailed name mType)
  where
    n `occursIn` ty = n `Set.member` freeMType ty
bindVariableTo name mType = return (Subst (M.singleton name mType))

unify :: (MType, MType) -> Infer Subst
unify (WFun a b, WFun c d) = unifyBinary (a, b) (c, d)
unify (WBoardF a, WBoardF b) = unify (a, b)
unify (WVar v, x) = v `bindVariableTo` x
unify (x, WVar v) = v `bindVariableTo` x
unify (WConst a, WConst b) | a == b = return mempty
unify (WList a, WList b) = unify (a, b)
unify (a, b) = throw (CannotUnify a b)

-- Unification of binary type constructors, such as functions and Either.
-- Unification is first done for the first operand, and assuming the
-- required substitution, for the second one.
unifyBinary :: (MType, MType) -> (MType, MType) -> Infer Subst
unifyBinary (a,b) (x,y) = do
    s1 <- unify (a, x)
    s2 <- unify (applySubst s1 (b, y))
    pure (s1 <> s2)

addSuffix x n = x <> T.pack (show n)


-- Ensure a type is using lowest possible names from the alphabet
-- TODO: There's probably a Functor implementation of this that would be nicer
recode :: MType -> MType
recode x =
  relabel (M.fromList $ zip (nub . extractVarNames $ x) typeNames) x

  where
    relabel names (WVar x) = WVar (fromJust $ M.lookup x names)
    relabel names (WFun x y) = WFun (relabel names x) (relabel names y)
    relabel names (WList x) = WList (relabel names x)
    relabel _ x = x

    extractVarNames :: MType -> [Name]
    extractVarNames (WVar x) = [x]
    extractVarNames (WFun x y) = extractVarNames x <> extractVarNames y
    extractVarNames (WConst _) = []
    extractVarNames (WList x) = extractVarNames x

fresh :: Infer MType
fresh = drawFromSupply >>= \case
          Right name -> pure (WVar name)
          Left err   -> throw err
  where
    drawFromSupply = Infer (do
      s:upply <- lift get
      lift (put upply)
      pure (Right s))

extendEnv (WEnv env) (name, pType) = WEnv (M.insert name pType env)

infer :: WEnv -> UExpr -> Infer (Subst, MType)
infer env (UConst (UInt _)) = wconst "Int"
infer env (UConst (UString _)) = wconst "String"
infer env (UConst (UBool _)) = wconst "Bool"
infer env (UConst (ULocation _)) = wconst "Location"
infer env (UConst (UList [])) = do
  tau <- fresh

  pure (mempty, WList tau)

infer env (UConst (UList (a:as))) = do
  (s, thisMType) <- infer env a
  (s2, restMType) <- infer env (UConst (UList as))

  s3 <- unify (applySubst s2 (WList thisMType), restMType)

  pure (s3 <> s2 <> s, WList $ applySubst s3 thisMType)

infer env (UConst (UBoardFunc _ exp)) = do
  -- TODO: Require exp type to be Action?
  (s, tau) <- infer (env <> boardFuncTypeEnv) exp

  pure (s, WBoardF tau)

infer env (UConst (UFunc _ name exp)) = do
  tau <- fresh
  let sigma = Forall mempty tau
      env' = extendEnv env (name, sigma)
  (s, tau') <- infer env' exp

  let fnTau = WFun (applySubst s tau) tau'
  --traceM . show $ (s, tau')

  pure (s, WFun (applySubst s tau) tau')

-- A sequence can contain heterogenous types. Only the final one is used.
infer env (USequence []) = do
  tau <- fresh
  pure (mempty, tau)
infer env (USequence [x]) = infer env x
infer env (USequence (x:xs)) = do
  -- Since Sequences "pass forward" their environment, we need access to the
  -- modified environment in infer.
  (s1, t1, env') <- inferWithNewEnv env x
  (s2, t2) <- infer env' $ USequence xs

  pure (s2, t2)

infer env (UDef _ _) = pure (mempty, "Void")

infer env (UApp f x) = do
  (s1, fTau) <- infer env f
  (s2, xTau) <- infer (applySubst s1 env) x
  fxTau <- fresh
  s3 <- unify (applySubst s2 fTau, WFun xTau fxTau)
  --traceM . T.unpack $ "Unified\n  " <> showType (applySubst s2 fTau) <> "\n  " <> showType (WFun xTau fxTau) <> "\n  " <> 
  --   showType (applySubst s3 (WFun xTau fxTau))
  let s = s3 <> s2 <> s1
  pure (s, applySubst s3 fxTau)

infer env (UVar name) = do
  sigma <- lookupEnv env name
  tau <- instantiate sigma
  --traceM . T.unpack $ "Instantiated " <> showT name <> ": " <> showType tau

  return (mempty, tau)
infer env (ULet (name, e0) e1) = do
  (s1, tau) <- infer env e0
  let env' = applySubst s1 env
  let sigma = generalize env' tau
  let env'' = extendEnv env' (name, sigma)
  (s2, tau') <- infer env'' e1

  pure (s2 <> s1, tau')

infer env (UIf cond lhs rhs) = do
  (scond, tau) <- infer env cond
  (slhs, lhsTau) <- infer (applySubst scond env) lhs
  (srhs, rhsTau) <- infer (applySubst slhs env) rhs


  s4 <- unify (tau, WConst "Bool")
  s5 <- unify (lhsTau, rhsTau)

  let s1 = scond
  let s2 = slhs
  let s3 = srhs
  let allSubs = s5 <> s4 <> s3 <> s2 <> s1

  pure (allSubs, applySubst s5 lhsTau)

infer env x = error . show $ "Unknown form in infer: " <> show x

inferWithNewEnv :: WEnv -> UExpr -> Infer (Subst, MType, WEnv)
inferWithNewEnv env (UDef name e) = do
  (s1, t1) <- infer env e

  let env' = applySubst s1 env
  let sigma = generalize env' t1
  let env'' = extendEnv env' (name, sigma)

  pure (s1, t1, applySubst s1 env'')

inferWithNewEnv env x = do
  (s, t) <- infer env x

  pure (s, t, applySubst s env)

generalize :: WEnv -> MType -> PType
generalize env mType = Forall qs mType
  where
    qs = freeMType mType `Set.difference` freeEnv env

lookupEnv :: WEnv -> Name -> Infer PType
lookupEnv (WEnv env) name = case M.lookup name env of
  Just x -> return x
  Nothing -> throw $ UnknownIdentifier name

instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
  subst <- substituteAllWithFresh qs
  return (applySubst subst t)

  where
    substituteAllWithFresh xs = do
      xs' <- sequenceA . fmap freshTuple . toList $ xs
      pure (Subst $ M.fromList xs')
    freshTuple x = do
      v <- fresh

      return (x, v)

wconst x = return (mempty, WConst x)
