{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardLang.BuiltIn where

import           Control.Lens         (element)
import           Control.Lens         (Traversal')
import           Control.Monad.Except (throwError)
import           Control.Monad.State  (get, modify)

import CardLang.Evaluator hiding (argAt)
import Utils
import Types

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

addPlayEffect = do
  env <- get

  effect   <- argAt 0
  template <- argAt 1

  action <- eval effect

  case fromU action of
    Right action' -> return . toUConst $ set playCode action' template
    Left x        -> throwError x

concat = do
  es :: [UExpr] <- argAt 0
  vs :: [UValue] <- traverse eval es

  case mapM fromU vs of
    Right vs' -> return . UConst . UList $ mconcat vs'
    Left y    -> throwError $ "Unexpected error in concat: " <> showT y

reduce = do
  f       <- argAt 0
  initial <- argAt 1
  xs      <- argAt 2

  foldM (folder f) initial (xs :: [UExpr])

  where
    folder :: UExpr -> UExpr -> UExpr -> EvalMonad UExpr
    folder f accum x = UConst <$> eval (UApp (UApp f accum) x)

binOp :: (FromU a, ToU b) => (a -> a -> b) -> EvalMonad UExpr
binOp f = toUConst <$> (f <$> argAt 0 <*> argAt 1)

cardsAt = do
  loc <- argAt 0

  cards <- view (cardsAtLocation loc) <$> currentBoard

  return . UConst . UList . fmap (UConst . USpecificCard) $
    zip (repeat loc) [0..length cards -1]

cardAttr :: ToU a => Traversal' Card a -> EvalMonad UExpr
cardAttr lens = do
  sloc@(location, index) <- argAt 0

  attr <- preview (cardsAtLocation location . ix index . cardTemplate . lens) <$> currentBoard

  case attr of
    Just c -> return . toUConst $ c
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)

chooseCard = do
  pid <- currentPlayer

  desc      <- argAt 0
  fromExprs <- argAt 1
  onChoose  <- argAt 2
  onPass    <- argAt 3

  from <- traverse eval fromExprs

  case sequence $ fmap fromU from of
    Right from' -> return . toUConst $ ActionChooseCard desc from' onChoose onPass
    Left y -> throwError y

compose = do
  f1 <- argAt 0
  f2 <- argAt 1
  x  <- argAt 2

  return $ UApp f1 (UApp f2 x)

currentBoard = do
  board <- view envBoard <$> get

  case board of
    Nothing -> throwError "Board function called outside of context"
    Just b -> return b

currentPlayer :: EvalMonad PlayerId
currentPlayer = do
  board <- view envBoard <$> get

  case board of
    Nothing -> throwError "Board function called outside of context"
    Just b -> do
      case preview (players . element 0 . playerId) b of
        Nothing -> throwError "No current player"
        Just p -> return p

makeHero = do
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
      upure ()

    Left y -> throwError y
