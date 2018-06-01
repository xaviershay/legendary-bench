{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module QueryTest where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer (runWriterT)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Except (runExceptT, ExceptT, throwError)
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Set             as Set

import Data.SCargot
import Data.SCargot.Repr.Basic
import Control.Applicative ((<|>))
import Text.Parsec (anyChar, char, digit, many1, manyTill, newline, satisfy, string, alphaNum)
import Text.Parsec.Text (Parser)

import Types
import Evaluator
import GameMonad
import Utils
import Debug.Trace

runQuery :: Show a => Term a -> a
runQuery q =
  let state = GameMonadState { _board = mkBoard } in
  let (Right result, log) = runIdentity $ runWriterT (runReaderT (runExceptT $ query q) state) in

  result

data UExpr =
    UConst UValue
  | UVar T.Text
  | ULet (T.Text, UExpr) UExpr
  | UApp UExpr UExpr
  | UBuiltIn T.Text
  deriving (Show, Eq)

type UEnv = M.HashMap T.Text UExpr

data Atom = AInt Int | ASymbol T.Text deriving (Show, Eq)

data UValue =
   UNone
 | ULocation Location
 | UInt SummableInt
 | UBool Bool
 | UFunc UEnv T.Text UExpr
 | UError T.Text

 deriving (Eq, Show)

data MType =
    WVar T.Text
  | WConst T.Text
  | WFun MType MType

  deriving (Eq, Show)

showType (WVar x) = x
showType (WConst x) = x
showType (WFun x y) = showType x <> " -> " <> showType y

newtype Subst = Subst (M.HashMap T.Text MType)

instance Monoid Subst where
  mempty = Subst M.empty
  mappend s1@(Subst a) s2@(Subst b) = Subst (a `M.union` b')
    where
      Subst b' = applySubst s1 s2

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

freeMType :: MType -> Set.Set T.Text
freeMType (WConst _) = mempty
freeMType (WVar x) = Set.singleton x
freeMType (WFun x y) = freeMType x <> freeMType y

data PType = Forall (Set.Set T.Text) MType

freePType :: PType -> Set.Set T.Text
freePType (Forall qs mType) = freeMType mType `Set.difference` qs

instance Substitutable PType where
  -- Remove the free types from the env and then substitute mType with that
  applySubst (Subst s) (Forall qs mType) =
    let qs' = M.fromList (fmap (\t -> (t, ())) $ toList qs)
        s'  = Subst (s `M.difference` qs')
    in Forall qs (applySubst s' mType)

newtype WEnv = WEnv (M.HashMap T.Text PType)

freeEnv :: WEnv -> Set.Set T.Text
freeEnv (WEnv e) = Set.unions (fmap freePType $ M.elems e)

instance Substitutable WEnv where
  applySubst s (WEnv env) = WEnv (M.map (applySubst s) env)

newtype Infer a = Infer (ExceptT InferError (State [T.Text]) a)
  deriving (Functor, Applicative, Monad)

data InferError =
    CannotUnify MType MType
  | OccursCheckFailed T.Text MType
  deriving (Show, Eq)

runInfer :: Infer a -> Either InferError a
runInfer (Infer inf) =
  evalState (runExceptT inf) (infiniteSupply alphabet)
  where
    alphabet = map T.singleton ['a'..'z']

    -- [a, b, c] ==> [a,b,c, a1,b1,c1, a2,b2,c2, …]
    infiniteSupply supply = supply <> addSuffixes supply (1 :: Integer)
      where
        addSuffixes xs n = map (\x -> addSuffix x n) xs <> addSuffixes xs (n+1)

throw :: InferError -> Infer a
throw = Infer . throwError

bindVariableTo :: T.Text -> MType -> Infer Subst
bindVariableTo name (WVar v) | name == v = return mempty
bindVariableTo name mType | name `occursIn` mType = throw (OccursCheckFailed name mType)
  where
    n `occursIn` ty = n `Set.member` freeMType ty
bindVariableTo name mType = return (Subst (M.singleton name mType))

unify :: (MType, MType) -> Infer Subst
unify (WFun a b, WFun c d) = unifyBinary (a, b) (c, d)
unify (WVar v, x) = v `bindVariableTo` x
unify (x, WVar v) = v `bindVariableTo` x
unify (WConst a, WConst b) | a == b = return mempty
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


extendEnv (WEnv env) (name, pType) = WEnv (M.insert name pType env)

infer :: WEnv -> UExpr -> Infer (Subst, MType)
infer env (UConst (UInt _)) = wconst "Int"
infer env (UConst (UBool _)) = wconst "Bool"
infer env (UConst (ULocation _)) = wconst "Location"
infer env (UConst (UFunc _ name exp)) = do
  tau <- fresh
  let sigma = Forall mempty tau
      env' = extendEnv env (name, sigma)
  (s, tau') <- infer env' exp

  pure (s, WFun (applySubst s tau) tau')

infer env (UApp f x) = do
  (s1, fTau) <- infer env f
  (s2, xTau) <- infer (applySubst s1 env) x
  fxTau <- fresh
  s3 <- unify (applySubst s2 fTau, WFun xTau fxTau)
  let s = s3 <> s2 <> s1
  pure (s, applySubst s3 fxTau)

infer env (UVar name) = do
  sigma <- lookupEnv env name
  tau <- instantiate sigma

  return (mempty, tau)
infer env (ULet (name, e0) e1) = do
  (s1, tau) <- infer env e0
  let env' = applySubst s1 env
  let sigma = generalize env' tau
  let env'' = extendEnv env' (name, sigma)
  (s2, tau') <- infer env'' e1

  pure (s2 <> s1, tau')

infer env x = error . show $ x

generalize :: WEnv -> MType -> PType
generalize env mType = Forall qs mType
  where
    qs = freeMType mType `Set.difference` freeEnv env

lookupEnv :: WEnv -> T.Text -> Infer PType
lookupEnv (WEnv env) name = case M.lookup name env of
  Just x -> return x
  Nothing -> error "Name not found"

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

printValue (UConst x) = case x of
                          (UInt (Sum n)) -> showT n
                          x -> showT x
printValue x = showT x

fresh :: Infer MType
fresh = drawFromSupply >>= \case
          Right name -> pure (WVar name)
          Left err   -> throw err
  where
    drawFromSupply = Infer (do
      s:upply <- lift get
      lift (put upply)
      pure (Right s))

pAtom :: Parser Atom
pAtom =  ((AInt . read) <$> many1 digit)
     <|> ((ASymbol . T.pack) <$> many1 alphaNum)

uQuery :: UEnv -> UExpr -> UValue
uQuery env (UConst fn@(UFunc env' x body)) = UFunc (env' <> env) x body
uQuery env (UConst v) = v
uQuery env (UVar label) = uQuery env $ M.lookupDefault (UConst . UError $ "Unknown variable: " <> label) label env
uQuery env (ULet (key, value) expr) = uQuery (M.insert key value env) expr
uQuery env (UApp fexp arg) =
  case uQuery env fexp of
    (UFunc env' argname body) -> uQuery (env <> env') $ ULet (argname, arg) body
    _ -> UError $ (printValue fexp) <> " is not a function"
uQuery env (UBuiltIn "add") = uQuery env $ builtInAdd env

convertLet :: [SExpr Atom] -> SExpr Atom -> Either String UExpr
convertLet [] f = toExpr f
convertLet (A (ASymbol k):v:vs) f = do
  value <- toExpr v
  body <- convertLet vs f

  return $ ULet (k, value) body
convertLet vs f = fail $ "Invalid let params: " <> show vs


-- TODO: Something something env capture
convertFn :: [SExpr Atom] -> SExpr Atom -> Either String UExpr
convertFn [] f = toExpr f
convertFn (A (ASymbol x):xs) f = do
  body <- convertFn xs f

  return . UConst $ UFunc mempty x body

-- TODO: Handle expr in first element
convertApp [A (ASymbol x)] = return . UVar $ x
convertApp [a] = toExpr a
convertApp (a:rest) = do
  xs <- convertApp rest
  a'   <- toExpr a

  return $ UApp xs a'

toExpr :: SExpr Atom -> Either String UExpr
toExpr (A (AInt x)) = Right . UConst . UInt . Sum $ x
toExpr (A (ASymbol "let") ::: L ls ::: f ::: Nil) = convertLet ls f
toExpr (A (ASymbol "fn") ::: L vs ::: f ::: Nil) = convertFn vs f
toExpr (A (ASymbol x)) = Right . UVar $ x
toExpr (L args) = convertApp (reverse args)

toExpr (L _) = return $ UConst UNone

myParser = setCarrier toExpr $ mkParser pAtom

lQuery :: UEnv -> T.Text -> UValue
lQuery env text = case decode myParser text of
                    Right x -> uQuery env (head x)
                    Left y -> error $ show y

inferType :: T.Text -> T.Text
inferType text = let Right result = showType <$> case decode myParser text of
                                      Right x -> snd <$> runInfer (infer builtInTypeEnv (head x))
                                      Left y  -> error "parse error" in result

builtInEnv :: UEnv
builtInEnv = M.mapWithKey (typeToFn 0) builtIns

typeToFn :: Int -> T.Text -> (MType, UEnv -> UExpr) -> UExpr
typeToFn n key (WFun a b, f) = UConst $ UFunc mempty ("a" <> showT n) (typeToFn (n+1) key (b, f))
typeToFn n key (WConst _, _) = UBuiltIn key

builtInTypeEnv :: WEnv
builtInTypeEnv = WEnv $ M.map (Forall mempty . fst) builtIns

builtIns :: M.HashMap T.Text (MType, UEnv -> UExpr)
builtIns = M.fromList
  [ ("add", (WFun (WConst "Int") (WFun (WConst "Int") (WConst "Int")), builtInAdd))
  ]

builtInAdd :: UEnv -> UExpr
builtInAdd env = let
  Just (UConst (UInt x)) = M.lookup "a0" env
  Just (UConst (UInt y)) = M.lookup "a1" env
  in UConst . UInt $ x + y

testInfer expected input =
  testCase (T.unpack $ input <> " :: " <> expected) $ expected @=? inferType input

test_TypeInference = testGroup "Type Inference"
  [ testInfer "Int" "1"
  , testInfer "Int" "(let (x 1) x)"
  , testInfer "Int" "(let (x (let (y 1) y)) x)"
  , testInfer "a -> Int" "(fn (x) 1)"
  , testInfer "Int" "((fn (x) 1) 2)"
  , testInfer "Int" "(add 1 2)"
  , testInfer "Int -> Int" "(add 1)"
  , testInfer "Int -> Int -> Int" "(fn (x y) (add x y))"
  ]

testEval = testEvalWith mempty
testEvalWith env expected input =
  testCase (T.unpack input) $ expected @=? lQuery (builtInEnv <> M.fromList env) input

test_ListQuery = testGroup "List Query"
  [ testEval (UInt 1) "1"
  , testEval (UError "Unknown variable: x") "x"
  , testEvalWith [("x", UConst . UInt $ 1)] (UInt 1) "x"
  , testEvalWith [("x", UConst . UInt $ 0)] (UInt 1) "(let (x 1) x)"
  , testEval (UInt 2) "(let (x 1 y 2) y)"
  , testEval (UInt 1) "((fn (x) x) 1)"
  , testEval (UInt 1) "(let (f (fn () 1)) f)"
  , testEval (UInt 1) "(let (f (fn (x) x)) (f 1))"
  , testEval (UInt 2) "(let (f (fn (x y) y)) (f 1 2))"
  , testEval (UInt 2) "(let (f (fn (x y) y)) ((f 1) 2))"
  , testEval (UError "1 is not a function") "(1 2)"
  , testEval (UInt 1) "((let (f (fn (x y) x)) (f 1)) 2)"
  , testEval (UInt 1) "(let (y 1) (let (f (fn () y)) f))"
  , testEval (UInt 3) "(add 1 2)"
  , testEval (UInt 3) "((add 1) 2)"
  ]

--focus = defaultMain test_TypeInference
focus = defaultMain test_ListQuery
