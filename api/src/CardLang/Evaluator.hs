{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module CardLang.Evaluator
  ( 
    evalWith
  , evalCards
  , evalWithBoard
  , builtIns
  , fromU
  , toU
  , showCode
  )
  where

import           Control.Lens         (at, element, ix, non, over, preview, set,
                                       view, Traversal')
import           Control.Monad        (foldM, forM)
import           Control.Monad.Reader (runReaderT, ask, local)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.State  (State, evalState, execState, get, modify,
                                       put, runState, withStateT)
import qualified Data.HashMap.Strict  as M
import           Data.Maybe           (fromJust)
import           Data.Sequence        (Seq, (<|))
import qualified Data.Text            as T
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

evalWithBoard :: Board -> UExpr -> UValue
evalWithBoard b = evalWith (set envBoard (Just b) emptyEnv)

evalCards :: UExpr -> Seq Card
evalCards exp = let env = builtInEnv in
    view envCards $ execState (runReaderT (runExceptT (eval exp)) 0) env

evalWith :: UEnv -> UExpr -> UValue
evalWith env exp = case evalState (runReaderT (runExceptT (eval exp)) 0) (set envBuiltIn (view envBuiltIn builtInEnv) env) of
                     Right x -> x
                     Left y -> UError y

showCode (UConst (UFunc  env name expr)) = "(fn {" <> T.intercalate ", " (fmap f . M.toList $ view envVariables env) <> "} [" <> name <> "] " <> showCode expr <> ")"
  where
    f (k, v) = k <> ": " <> showCode v
showCode (UConst (UBoardFunc _ expr)) = "@" <> showCode expr
showCode (UConst (UInt (Sum x))) = showT x
showCode (UConst (UString x)) = x
showCode (UConst (UList xs)) = "[" <> T.intercalate " " (fmap showCode xs) <> "]"
showCode (UConst x) = "!!! UNKNOWN " <> showT (UConst x)
showCode (UVar x) = "VAR " <> x
showCode (UApp e1 e2) = "APP " <> showSExpr [showCode e1, showCode e2]
showCode (ULet (name,  value) expr) = "LET " <> "(let [" <> name <> " " <> showCode value <> "] " <> showCode expr <> ")"
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
  --traceM ">> ====="
  --traceM . T.unpack $ showT level <> ": " <> (T.replicate level " ") <> showCode expr <> "  | " <> showEnvOneLine vars
  --traceM . T.unpack $ "About to " <> showT level <> ": " <> (T.replicate level " ") <> showCode expr
  --traceM "Env before:"
  --traceEnv
  --traceM ""
  if level > 1000 then
    throwError "Execution exceeded stack"
  else
    do
      r <- local (+ 1) $ eval' expr

      --traceMT $ showT level <> ": " <> (T.replicate level " ") <> "==> " <> showCode (UConst r)
   --   traceM "Env after:"
   --   traceEnv
   --   traceM "<< ====="

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

eval' (UConst fn@(UFunc env' x body)) = do
  env <- get
  pure $ UFunc (extendEnv (view envVariables env') env) x body
eval' (UConst fn@(UBoardFunc env' expr)) = do
  env <- get
  board <- view envBoard <$> get

  case board of
    Nothing -> pure (UBoardFunc (extendEnv (view envVariables env') env) expr)
    Just b -> do
      modify (extendEnv (view envVariables env'))
      eval expr
      -- TODO: Put env back the way we found it?
    

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

  case fn of
    (UFunc env' argname body) -> do
      withVars (view envVariables env') $ eval (ULet (argname, arg) body)

    x -> pure . UError $ (showT x) <> " is not a function"

eval' (UIf cond lhs rhs) = do
  UBool result <- eval cond

  if result then
    eval lhs
  else
    eval rhs

eval' (UBuiltIn x) = do
  expr <- snd . fromJust $ M.lookup x builtIns

  eval expr

builtInEnv :: UEnv
builtInEnv = set envBuiltIn (M.mapWithKey (typeToFn 0) builtIns) emptyEnv
  where
    typeToFn :: Int -> Name -> BuiltIn -> UExpr
    typeToFn n key (WFun a b, f) = UConst $ UFunc emptyEnv ("_a" <> showT n) (typeToFn (n+1) key (b, f))
    typeToFn n key (WBoardF mtype, _) = UBuiltIn key
    typeToFn n key (WList _, _) = UBuiltIn key
    typeToFn n key (WConst _, _) = UBuiltIn key
    typeToFn n key (WVar _, _) = UBuiltIn key
    typeToFn n key (x, _) = error $ "Unknown in typeToFn: " <> show x

builtIns :: M.HashMap Name BuiltIn
builtIns = M.fromList
  [ ("add", ("Int" ~> "Int" ~> "Int", builtInAdd))
  , ("+", ("Int" ~> "Int" ~> "Int", builtInAdd))
  , ("double", ("Int" ~> "Int", builtInDouble))
  , ("=<", ("Int" ~> "Int" ~> "Bool", builtInComparison (<=)))
  , ("<=", ("Int" ~> "Int" ~> "Bool", builtInComparison (<=)))
  , ("<", ("Int" ~> "Int" ~> "Bool", builtInComparison (<)))
  , (">", ("Int" ~> "Int" ~> "Bool", builtInComparison (>)))
  , (">=", ("Int" ~> "Int" ~> "Bool", builtInComparison (>=)))
  , ("=>", ("Int" ~> "Int" ~> "Bool", builtInComparison (>=)))
  , ("==", ("a" ~> "a" ~> "Bool", builtInEq))
  -- TODO: These variables likely not good enough? Probably able to clash with
  -- other reduce that should resolve differently! Could fix by making this
  -- type statement run in the Infer monad to generate fresh tau each time.
  -- EDIT: No actually we probably just need to a) Have proper forall binding, b) instantiate it
  -- EDIT2: Yup, currently hard coded in TypeInference.hs, need to fix
  , ("reduce", (("_b" ~> "_a" ~> "_b") ~> "_b" ~> WList "_a" ~> "_b", builtInReduce))
  , ("concat", (WList (WList "_x") ~> WList "_x",  builtInConcat))

  , ("noop", ("Action", builtInNoop))
  , ("attack", ("Int" ~> "Action", builtInAttack))
  , ("recruit", ("Int" ~> "Action", builtInRecruit))
  , ("rescue-bystander", ("Int" ~> "Action", builtInRescue))
  , ("current-player", ("PlayerId", builtInCurrentPlayer))
  , ("reveal", ("SpecificCard" ~> "Action", builtInReveal))
  , ("ko", ("SpecificCard" ~> "Action", builtInKo))
  , ("draw", ("Int" ~> "Action", builtInDraw))
  , ("move", ("SpecificCard" ~> "Location" ~> "Action", builtInMove))
  , ("card-location", ("Location" ~> "Int" ~> "SpecificCard", builtInCardLocation))
  , ("card-cost", ("SpecificCard" ~> "Int", builtInCardAttribute heroCost))
  , ("card-type", ("SpecificCard" ~> "String", builtInCardAttribute heroType))
  , ("cards-at", ("Location" ~> WList "SpecificCard", builtInCardsAt))
  , ("player-location", ("PlayerId" ~> "String" ~> "Location", builtInPlayerLocation))
  , ("combine", ("Action" ~> "Action" ~> "Action", builtInCombine))
  , ("choose-card", ("String" ~> WList "SpecificCard" ~> ("SpecificCard" ~> "Action") ~> "Action" ~> "Action", builtInChooseCard))
  , ("add-play-effect", (WBoardF "Action" ~> "CardTemplate" ~> "CardTemplate", builtInAddPlayEffect))
  , ("make-hero-full", ("String"
                     ~> "String"
                     ~> "String"
                     ~> "String"
                     ~> "Int"
                     ~> "Int"
                     ~> "String"
                     ~> ("CardTemplate" ~> "CardTemplate")
                     ~> "Void",
                     builtInMakeHeroFull))
  ]

returnConst :: FromU a => a -> EvalMonad UExpr
returnConst = return . UConst . toU

builtInConcat = do
  xs :: [UExpr] <- argAt 0
  xs' :: [UValue] <- sequence . fmap eval $ xs

  case sequence $ map fromU xs' of
    Right (xs'' :: [[UExpr]]) -> return . UConst . UList $ mconcat xs''
    Left y -> throwError $ "Unexpected error in concat: " <> showT y


builtInReduce = do
  f       <- argAt 0
  initial <- argAt 1
  xs      <- argAt 2

  let f' = f :: UExpr
  let initial' = UConst initial
  let xs' = xs :: [UExpr]

  value <- foldM (folder f') initial' xs

  return value

  where
    folder :: UExpr -> UExpr -> UExpr -> EvalMonad UExpr
    folder f accum x = UConst <$> eval (UApp (UApp f accum) x)

builtInCardsAt = do
  loc <- argAt 0

  cards <- view (cardsAtLocation loc) <$> currentBoard

  return . UConst . UList . fmap (UConst . USpecificCard) $ zip (repeat loc) [0..length cards -1]

builtInCardAt = do
  sloc@(location, index) <- argAt 0

  card <- preview (cardsAtLocation location . ix index) <$> currentBoard

  case card of
    Just c -> return . UConst . toU $ view cardTemplate c
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)

builtInChooseCard = do
  pid <- currentPlayer

  desc <- argAt 0
  fromExprs <- argAt 1
  onChoose <- argAt 2
  onPass <- argAt 3

  from <- sequence . fmap eval $ fromExprs

  case sequence $ fmap fromU from of
    Right from' -> returnConst $ ActionChooseCard desc from' onChoose onPass
    Left y -> throwError y

builtInCardAttribute :: FromU a => Traversal' Card a -> EvalMonad UExpr
builtInCardAttribute lens = do
  sloc@(location, index) <- argAt 0

  attr <- preview (cardsAtLocation location . ix index . cardTemplate . lens) <$> currentBoard

  case attr of
    Just c -> return . UConst . toU $ c
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)

builtInCurrentPlayer = UConst . UPlayerId <$> currentPlayer
  
builtInReveal = do
  location <- argAt 0

  returnConst $ ActionReveal (TConst location)

builtInKo = do
  location <- argAt 0

  returnConst $ ActionKO location

builtInDraw = do
  pid <- currentPlayer
  amount <- argAt 0

  returnConst . mconcat . replicate amount $ ActionDraw pid

builtInMove = do
  from <- argAt 0
  to <- argAt 1

  returnConst $ ActionMove (TConst from) (TConst to) (TConst Front)

builtInNoop = return . UConst . UAction $ ActionNone

builtInCombine = do
  a <- argAt 0
  b <- argAt 1

  returnConst $ a <> (b :: Action)

builtInCardLocation = do
  loc <- argAt 0
  index <- argAt 1

  returnConst $ ((loc, index) :: SpecificCard)

builtInPlayerLocation = do
  pid <- argAt 0
  sloc <- argAt 1

  return . UConst . ULocation $ PlayerLocation pid sloc


builtInAddPlayEffect :: EvalMonad UExpr
builtInAddPlayEffect = do
  env <- get

  effect   <- argAt 0
  template <- argAt 1

  action <- eval effect

  case fromU action of
    Right action' -> return . UConst . UCardTemplate $ set playCode action' template
    Left x -> throwError x
  

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


builtInAdd :: EvalMonad UExpr
builtInAdd = do
  x <- argAt 0
  y <- argAt 1

  return . UConst . UInt $ x + y

builtInDouble :: EvalMonad UExpr
builtInDouble = do
  traceEnv

  x <- argAt 0

  return . UConst . UInt $ x * 2

builtInComparison f = do
  x <- argAt 0
  y <- argAt 1

  return . UConst . UBool $ f x (y :: Int)

builtInEq = do
  x <- argAt 0
  y <- argAt 1

  return . UConst . UBool $ x == (y :: UValue)

builtInAttack :: EvalMonad UExpr
builtInAttack = do
  amount <- argAt 0
  pid    <- currentPlayer

  return . UConst . UAction $ ActionAttack2 pid amount

builtInRecruit :: EvalMonad UExpr
builtInRecruit = do
  amount <- argAt 0
  pid    <- currentPlayer

  return . UConst . UAction $ ActionRecruit pid amount

builtInRescue = do
  amount <- argAt 0
  pid    <- currentPlayer

  return . UConst . UAction $ ActionRecruit pid amount
  returnConst (ActionAllowFail $ ActionMove
                    (TSpecificCard (TConst BystanderDeck) (TConst 0))
                    (TPlayerLocation (TConst pid) (TConst Victory))
                    (TConst Front)
                  )

builtInMakeHeroFull = do
  name     <- argAt 0
  team     <- argAt 1
  ability  <- argAt 2
  htype    <- argAt 3
  cost     <- argAt 4
  amount   <- argAt 5
  desc     <- argAt 6
  callback <- argAt 7

  let template = UConst . UCardTemplate $ HeroCard
                  { _heroName = name
                  , _heroAbilityName = ability
                  , _heroType = HeroType htype
                  , _heroTeam = HeroTeam team
                  , _heroCost = cost
                  , _heroStartingNumber = amount
                  , _heroDescription = desc
                  , _playEffect = ActionNone
                  , _playCode = UConst . UAction $ ActionNone
                  }

  template' <- eval (UApp callback template)

  case fromU template' of
    Right x -> do
      modify (over envCards (x <|))
      return . UConst $ UNone

    Left y -> throwError y

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
  toU :: a -> UValue

instance FromU SummableInt where
  fromU (UInt x) = return x
  fromU x        = throwError ("Expected UInt, got " <> showT x)
  toU = UInt

instance FromU Int where
  fromU (UInt (Sum x)) = return x
  fromU x        = throwError ("Expected UInt, got " <> showT x)
  toU = UInt . Sum

instance FromU T.Text where
  fromU (UString x) = return x
  fromU x        = throwError ("Expected UString, got " <> showT x)
  toU = UString

instance FromU Card where
  fromU (UCardTemplate x) = return x
  fromU x        = throwError ("Expected UCardTemplate, got " <> showT x)
  toU = UCardTemplate

instance FromU Action where
  fromU (UAction x) = return x
  fromU x        = throwError ("Expected UAction, got " <> showT x)
  toU = UAction

instance FromU SpecificCard where
  fromU (USpecificCard x) = return x
  fromU x        = throwError ("Expected USpecificCard, got " <> showT x)
  toU = USpecificCard

instance FromU Location where
  fromU (ULocation x) = return x
  fromU x        = throwError ("Expected ULocation, got " <> showT x)
  toU = ULocation

instance FromU ScopedLocation where
  fromU (UString "Deck") = fromU (UString "PlayerDeck")
  fromU (UString x) = return . read . T.unpack $ x
  fromU x        = throwError ("Expected UString, got " <> showT x)
  toU PlayerDeck = UString "Deck"
  toU x = UString . showT $ x

instance FromU HeroType where
  toU (HeroType x) = UString x

instance FromU PlayerId where
  fromU (UPlayerId x) = return x
  fromU x        = throwError ("Expected UPlayerId, got " <> showT x)

instance FromU UExpr where
  fromU x = return $ UConst x

instance FromU UValue where
  fromU = return

instance FromU [UExpr] where
  fromU (UList xs) = return xs
  fromU x        = throwError ("Expected UList, got " <> showT x)

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
