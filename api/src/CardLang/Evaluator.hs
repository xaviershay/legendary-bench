{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module CardLang.Evaluator
  (
    evalWith
  , evalCards
  , eval
  , currentVars
  , fromU
  , toU
  , toUConst
  , showCode
  , freeVars
  , FromU
  , ToU
  , argAt
  , upure
  , uliftA1
  , uliftA2
  , uliftA3
  )
  where

import Control.Applicative (liftA2, liftA3)
import           Control.Lens         (at, element, ix, non, over, preview, set,
                                       view, Traversal', filtered, folded)
import           Control.Monad        (foldM, forM)
import           Control.Monad.Reader (runReaderT, ask, local, Reader, runReader)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.State  (State, evalState, execState, get, modify,
                                       put, runState, withStateT)
import qualified Data.HashMap.Strict  as M
import           Data.Maybe           (fromJust)
import           Data.Sequence        (Seq, (<|))
import qualified Data.Text            as T
import qualified Data.Set                as Set
import           Text.Show.Pretty     (ppShow)
import Data.List (sortBy, sortOn)
import Data.Maybe (catMaybes)

import Types
import Utils

import CardLang.Types
import Debug.Trace

setNon lens value = set lens (Just value)

printValue (UConst x) = case x of
                          (UInt (Sum n)) -> showT n
                          x -> showT x
printValue x = showT x

evalCards :: UEnv -> UExpr -> Seq Card
evalCards env exp =
    view envCards $ execState (runReaderT (runExceptT (eval exp)) 0) env

evalWith :: UEnv -> UExpr -> UValue
evalWith env exp = case evalState (runReaderT (runExceptT (eval exp)) 0) env of
                     Right x -> x
                     Left y -> UError y

showCode (UConst (UFunc fn)) = "(fn {"
                                   <> T.intercalate ", " (fmap f . M.toList $ view fnBindings fn)
                                   <> "} ["
                                   <> (view fnArgName fn) <> "] "
                                   <> showCode (view fnBody fn) <> ")"
  where
    f (k, v) = k <> ": " <> showCode v
showCode (UConst (UBoardFunc bindings expr)) = "@{"
                                   <> T.intercalate ", " (fmap f . M.toList $ bindings)
                                   <> "} "
                                   <> showCode expr
  where
    f (k, v) = k <> ": " <> showCode v
showCode (UConst (UInt (Sum x))) = showT x
showCode (UConst (UString x)) = x
showCode (UConst UNone) = "()"
showCode (UConst (UList xs)) = "[" <> T.intercalate " " (fmap showCode xs) <> "]"
showCode (UConst x) = "!!! UNKNOWN " <> showT (UConst x)
showCode (UVar x) = x
showCode (UApp e1 e2) = showSExpr [showCode e1, showCode e2]
showCode (ULet (name,  value) expr) = "(let [" <> name <> " " <> showCode value <> "] " <> showCode expr <> ")"
showCode (UIf cond e1 e2) = showSExpr ["if", showCode cond, showCode e1, showCode e2]
showCode (UDef name expr) = showSExpr ["def", name, showCode expr]
showCode (USequence xs) = T.intercalate " " $ fmap showCode xs
showCode (UBuiltIn x) = "<" <> x <> ">"
showCode x = "!!! UNKNOWN: " <> showT x

showSExpr atoms = "(" <> T.intercalate " " atoms <>  ")"

withVars :: Bindings -> EvalMonad UValue -> EvalMonad UValue
withVars newEnv m = do
  oldVars <- currentVars
  modify (extendEnv newEnv)
  result <- m
  modify (set envVariables oldVars)
  pure $ result

eval :: UExpr -> EvalMonad UValue
eval expr = do
  level <- ask
  vars <- currentVars
  --traceM . T.unpack $ showT level <> ": " <> (T.replicate level " ") <> showCode expr <> "  | " <> showEnvOneLine vars
  if level > 10000 then
    throwError "Execution exceeded stack"
  else
    do
      r <- local (+ 1) $ eval' expr

      --traceMT $ showT level <> ": " <> (T.replicate level " ") <> "==> " <> showCode (UConst r)

      return r

eval' :: UExpr -> EvalMonad UValue
eval' (USequence []) = pure UNone
eval' (USequence [x]) = eval x
eval' (USequence (x:xs)) = do
  eval x
  eval (USequence xs)

eval' (UDef name expr) = do
  body <- eval expr
  modify (setNon (envVariables . at name) (UConst body))
  pure UNone

eval' e@(UConst fn@(UBoardFunc bindings expr)) = do
  env <- get
  board <- view envBoard <$> get

  case board of
    Nothing -> pure (UBoardFunc (bindVars env bindings) expr)
    Just b  -> withVars bindings $ eval expr

  where
    bindVars env b =
      let free = freeVars env e in
      let bindings = view envVariables env `M.intersection` M.fromList (zip (toList free) (repeat ())) in
      b `M.union` bindings

eval' e@(UConst (UFunc d)) = do
  env <- get
  pure $ UFunc (bindVars env d)

  where
    bindVars env d =
      let free = freeVars env e in
      let bindings = view envVariables env `M.intersection` M.fromList (zip (toList free) (repeat ())) in
      over fnBindings (\x -> x `M.union` bindings) d

eval' (UConst (UList xs)) = do
  xs' <- sequenceA (map eval xs)

  pure . UList . map UConst $ xs'

eval' (UConst v) = pure $ v
eval' (UVar label) = do
  env <- get
  let err = UError $ "Unknown variable: " <> label
  case catMaybes $ [view (envVariables . at label) env, view (envBuiltIn . at label) env] of
    (x:_) -> eval x
    [] -> return err

eval' (ULet (key, value) expr) = do
  oldVars <- currentVars

  value' <- UConst <$> eval value

  withVars (M.singleton key value) (eval expr)


eval' (UApp fexp arg) = do
  fn <- eval fexp
  arg' <- UConst <$> eval arg

  case fn of
    UFunc fn -> do
      withVars (view fnBindings fn) $ eval (ULet (view fnArgName fn, arg') (view fnBody fn))

    x -> pure . UError $ (showT x) <> " is not a function"

eval' (UIf cond lhs rhs) = do
  UBool result <- eval cond

  if result then
    eval lhs
  else
    eval rhs

eval' (UBuiltIn x) = do
  env <- get

  let def = fromJust $ view (envBuiltInDefs . at x) env
  let fn = view builtInFn def

  expr <- fn

  eval expr



returnConst :: ToU a => a -> EvalMonad UExpr
returnConst = return . UConst . toU

upure :: ToU a => a -> EvalMonad UExpr
upure = pure . toUConst

uliftA1 :: ToU b => (a -> b) -> EvalMonad a -> EvalMonad UExpr
uliftA1 x y = toUConst <$> fmap x y

uliftA2 :: ToU c => (a -> b -> c) -> EvalMonad a -> EvalMonad b -> EvalMonad UExpr
uliftA2 x y z = toUConst <$> liftA2 x y z

uliftA3 :: ToU d => (a -> b -> c -> d) -> EvalMonad a -> EvalMonad b -> EvalMonad c -> EvalMonad UExpr
uliftA3 w x y z = toUConst <$> liftA3 w x y z

builtInCardAttribute :: ToU a => Traversal' Card a -> EvalMonad UExpr
builtInCardAttribute lens = do
  specificCard <- argAt 0

  attr <- preview (cardAtLocation specificCard . cardTemplate . lens) <$> currentBoard

  case attr of
    Just c -> return . UConst . toU $ c
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT specificCard)

showEnvOneLine vars =
  T.intercalate ", " $ fmap f $ (sortOn fst $ M.toList vars)

  where
    f (n, x) = n <> " = " <> showCode x

traceEnv = do
  vars <- currentVars

  traceM ""
  forM (sortOn fst $ M.toList vars) $ \(n, x) -> do
    traceM . T.unpack $ n <> " = " <> showCode x
  traceM ""

builtInRescue = uliftA2 ActionRescueBystander currentPlayer (argAt 0)

currentVars = view envVariables <$> get
currentPlayer = do
  board <- view envBoard <$> get

  case board of
    Nothing -> throwError "Board function called outside of context"
    Just b -> do
      case preview (players . element 0 . playerId) b of
        Nothing -> throwError "No current player"
        Just p -> return p

currentBoard = do
  board <- view envBoard <$> get

  case board of
    Nothing -> throwError "Board function called outside of context"
    Just b -> return b

class FromU a where
  fromU :: UValue -> Either T.Text a

class ToU a where
  toU :: a -> UValue

instance FromU SummableInt where
  fromU (UInt x) = return x
  fromU x        = throwError ("Expected UInt, got " <> showT x)
instance ToU SummableInt where
  toU = UInt

instance FromU Int where
  fromU (UInt (Sum x)) = return x
  fromU x        = throwError ("Expected UInt, got " <> showT x)
instance ToU Int where
  toU = UInt . Sum

instance FromU T.Text where
  fromU (UString x) = return x
  fromU x        = throwError ("Expected UString, got " <> showT x)
instance ToU T.Text where
  toU = UString

instance FromU Card where
  fromU (UCardTemplate x) = return x
  fromU x        = throwError ("Expected UCardTemplate, got " <> showT x)

instance ToU Card where
  toU = UCardTemplate

instance FromU Action where
  fromU (UAction x) = return x
  fromU x        = throwError ("Expected UAction, got " <> showT x)
instance ToU Action where
  toU = UAction

instance FromU SpecificCard where
  fromU (USpecificCard x) = return x
  fromU x        = throwError ("Expected USpecificCard, got " <> showT x)
instance ToU SpecificCard where
  toU = USpecificCard

instance FromU Location where
  fromU (ULocation x) = return x
  fromU x        = throwError ("Expected ULocation, got " <> showT x)
instance ToU Location where
  toU = ULocation

instance FromU ScopedLocation where
  fromU (UString "Deck") = fromU (UString "PlayerDeck")
  fromU (UString x) = return . read . T.unpack $ x
  fromU x        = throwError ("Expected UString, got " <> showT x)
instance ToU ScopedLocation where
  toU PlayerDeck = UString "Deck"
  toU x = UString . showT $ x

instance ToU HeroType where
  toU (HeroType x) = UString x

instance ToU HeroTeam where
  toU (HeroTeam x) = UString x

instance FromU PlayerId where
  fromU (UPlayerId x) = return x
  fromU x        = throwError ("Expected UPlayerId, got " <> showT x)
instance ToU PlayerId where
  toU x = UPlayerId x

instance FromU UExpr where
  fromU x = return $ UConst x

instance FromU UValue where
  fromU = return

instance FromU [UExpr] where
  fromU (UList xs) = return xs
  fromU x        = throwError ("Expected UList, got " <> showT x)

instance ToU Bool where
  toU x = UBool x

instance FromU Bool where
  fromU (UBool x) = return x

instance ToU () where
  toU () = UNone

toUConst :: ToU a => a -> UExpr
toUConst = UConst . toU

argAt :: FromU a => Int -> EvalMonad a
argAt index = do
  let name = "_a" <> showT index

  vars <- currentVars

  case view (at name) vars of
                       Nothing -> throwError $ "Not in env: " <> showT name
                       Just x -> do
                         result <- eval x
                         case fromU result of
                           Right x -> return x
                           Left y -> throwError $ "Arg " <> name <> " was not of the right type: " <> showT y

freeVars :: UEnv -> UExpr -> Set.Set Name
freeVars env expr = runReader (freeVars' expr) env

freeVars' :: UExpr -> Reader UEnv (Set.Set Name)
freeVars' (UConst (UList xs)) = mconcat <$> traverse freeVars' xs
freeVars' (UConst (UBoardFunc _ fn)) = freeVars' fn
freeVars' (ULet (name, _) expr) = Set.delete name <$> freeVars' expr
freeVars' (UDef name expr)      = Set.delete name <$> freeVars' expr
freeVars' (UConst (UFunc fn))   = Set.delete name <$> freeVars' expr
  where
    name = view fnArgName fn
    expr = view fnBody fn
freeVars' (UApp e1 e2) = liftA2 (<>) (freeVars' e1) (freeVars' e2)
freeVars' (UIf e1 e2 e3) = mconcat <$> traverse freeVars' [e1, e2, e3]
freeVars' (USequence xs) = mconcat <$> traverse freeVars' xs
freeVars' (UConst _)   = pure mempty
freeVars' (UBuiltIn _) = pure $ Set.fromList . fmap (\x -> "_a" <> showT x) $ [0..10]
freeVars' (UVar x)     = pure $ Set.singleton x
