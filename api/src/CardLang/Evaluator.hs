{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CardLang.Evaluator
  ( eval
  , evalWith
  , evalCards
  , evalWithBoard
  , builtIns
  , fromU
  )
  where

import Text.Show.Pretty (ppShow)
import Control.Lens (view, set, at, non, element, preview, over, ix)
import Control.Monad.State (evalState, State, modify, put, get, withStateT, execState, runState)
import qualified Data.HashMap.Strict  as M
import qualified Data.Text  as T
import Data.Sequence ((<|), Seq)
import Data.Maybe (fromJust)
import Control.Monad.Except (throwError, runExceptT)

import Types
import Utils

import CardLang.Types
import Debug.Trace

setNon lens value = set lens (Just value)

printValue (UConst x) = case x of
                          (UInt (Sum n)) -> showT n
                          x -> showT x
printValue x = showT x

eval :: UExpr -> UValue
eval = evalWith mempty

evalWithBoard :: Board -> UExpr -> UValue
evalWithBoard b = evalWith (set envBoard (Just b) mempty)

evalCards :: UExpr -> Seq Card
evalCards exp = let env = builtInEnv in
    view envCards $ execState (runExceptT (evalWith' exp)) env

evalWith :: UEnv -> UExpr -> UValue
evalWith env exp = case evalState (runExceptT (evalWith' exp)) (builtInEnv <> env) of
                     Right x -> x
                     Left y -> UError y

evalWith' :: UExpr -> EvalMonad UValue
evalWith' (USequence []) = pure UNone
evalWith' (USequence [x]) = evalWith' x
evalWith' (USequence (x:xs)) = do
  evalWith' x
  evalWith' (USequence xs)

evalWith' (UDef name expr) = do
  body <- evalWith' expr
  modify (setNon (envVariables . at name) (UConst body))
  pure UNone

evalWith' (UConst fn@(UFunc env' x body)) = do
  env <- get

  pure $ UFunc (env' <> env) x body
evalWith' (UConst fn@(UBoardFunc env' expr)) = do
  env <- get
  board <- view envBoard <$> get

  case board of
    Nothing -> pure (UBoardFunc (env' <> env) expr)
    Just b -> do
      modify (env' <>)
      evalWith' expr -- TODO: Setup environment from board
      -- TODO: Put env back the way we found it?
    

evalWith' (UConst (UList xs)) = do
  xs' <- sequenceA (map evalWith' xs)

  pure . UList . map UConst $ xs'

evalWith' (UConst v) = pure $ v
evalWith' (UVar label) = do
  env <- get

  evalWith' $ view (envVariables . at label . non (UConst . UError $ "Unknown variable: " <> label)) env

evalWith' (ULet (key, value) expr) = do
  oldVars <- currentVars

  modify (setNon (envVariables . at key) value)
  result <- evalWith' expr

  modify (set envVariables oldVars)

  pure $ result

evalWith' (UApp fexp arg) = do
  fn <- evalWith' fexp

  case fn of
    (UFunc env' argname body) -> do
      modify (\x -> env' <> x) >> evalWith' (ULet (argname, arg) body)

    _ -> pure . UError $ (printValue fexp) <> " is not a function"

evalWith' (UIf cond lhs rhs) = do
  UBool result <- evalWith' cond

  if result then
    evalWith' lhs
  else
    evalWith' rhs

evalWith' (UBuiltIn x) = do
  expr <- snd . fromJust $ M.lookup x builtIns

  evalWith' expr

builtInEnv :: UEnv
builtInEnv = set envVariables (M.mapWithKey (typeToFn 0) builtIns) mempty
  where
    typeToFn :: Int -> Name -> BuiltIn -> UExpr
    typeToFn n key (WFun a b, f) = UConst $ UFunc mempty ("a" <> showT n) (typeToFn (n+1) key (b, f))
    typeToFn n key (WBoardF mtype, _) = UBuiltIn key
    typeToFn n key (WConst _, _) = UBuiltIn key

builtIns :: M.HashMap Name BuiltIn
builtIns = M.fromList
  [ ("add", ("Int" ~> "Int" ~> "Int", builtInAdd))
  , ("+", ("Int" ~> "Int" ~> "Int", builtInAdd))
  , ("=<", ("Int" ~> "Int" ~> "Bool", builtInComparison (<=)))
  , ("<=", ("Int" ~> "Int" ~> "Bool", builtInComparison (<=)))
  , ("<", ("Int" ~> "Int" ~> "Bool", builtInComparison (<)))
  , (">", ("Int" ~> "Int" ~> "Bool", builtInComparison (>)))
  , (">=", ("Int" ~> "Int" ~> "Bool", builtInComparison (>=)))
  , ("=>", ("Int" ~> "Int" ~> "Bool", builtInComparison (>=)))
  , ("==", ("Int" ~> "Int" ~> "Bool", builtInComparison (==)))
  , ("noop", ("Action", builtInNoop))
  , ("attack", ("Int" ~> "Action", builtInAttack))
  , ("recruit", ("Int" ~> "Action", builtInRecruit))
  , ("rescue-bystander", ("Int" ~> "Action", builtInRescue))
  , ("current-player", ("PlayerId", builtInCurrentPlayer))
  , ("reveal", ("SpecificCard" ~> "Action", builtInReveal))
  , ("move", ("SpecificCard" ~> "Location" ~> "Action", builtInMove))
  , ("card-location", ("Location" ~> "Int" ~> "SpecificCard", builtInCardLocation))
  , ("card-at", ("SpecificCard" ~> "CardTemplate", builtInCardAt))
  , ("card-cost", ("CardTemplate" ~> "Int", builtInCardAttribute heroCost))
  , ("player-location", ("PlayerId" ~> "String" ~> "Location", builtInPlayerLocation))
  , ("combine", ("Action" ~> "Action" ~> "Action", builtInCombine))
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

builtInCardAt = do
  sloc@(location, index) <- argAt 0

  card <- preview (cardsAtLocation location . ix index) <$> currentBoard

  case card of
    Just c -> return . UConst . toU $ view cardTemplate c
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)


builtInCardAttribute lens = do
  template <- argAt 0

  returnConst $ view lens template

builtInCurrentPlayer = UConst . UPlayerId <$> currentPlayer
  
builtInReveal = do
  location <- argAt 0

  returnConst $ ActionReveal (TConst location)

builtInMove = do
  from <- argAt 0
  to <- argAt 1

  returnConst $ ActionMove (TConst from) (TConst to) (TConst Front)

builtInNoop = return . UConst . toU $ ActionNone

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

  action <- evalWith' effect

  case fromU action of
    Right action' -> return . UConst . UCardTemplate $ set playCode action' template
    Left x -> throwError x
  
builtInAdd :: EvalMonad UExpr
builtInAdd = do
  x <- argAt 0
  y <- argAt 1

  return . UConst . UInt $ x + y

builtInComparison f = do
  x <- argAt 0
  y <- argAt 1

  return . UConst . UBool $ f x (y :: Int)

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

  template' <- evalWith' (UApp callback template)

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

instance FromU PlayerId where
  fromU (UPlayerId x) = return x
  fromU x        = throwError ("Expected UPlayerId, got " <> showT x)

instance FromU UExpr where
  fromU x = return $ UConst x

argAt :: FromU a => Int -> EvalMonad a
argAt index = do
  let name = "a" <> showT index

  vars <- currentVars

  case view (at name) vars of
                       Nothing -> throwError $ "Not in env: " <> showT name
                       Just x -> do
                         result <- evalWith' x
                         case fromU result of
                           Right x -> return x
                           Left y -> throwError $ "Arg " <> name <> " was not of the right type: " <> showT y
