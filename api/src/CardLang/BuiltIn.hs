{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardLang.BuiltIn where

import           Control.Lens         (element, Traversal')
import           Control.Monad.Except (throwError)
import           Control.Monad.State  (get, gets, modify)
import           Data.List            (nub)
import           Data.Sequence ((<|))
import qualified Data.Sequence        as S

import CardLang.Parser (parseUnsafe)
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

addPip attr fn suffix = do
  amount :: Int  <- argAt 0
  template <- argAt 1

  let action = ActionEval mempty $ UApp (UVar fn) (toUConst amount)

  return
    . toUConst
    . set attr (Just $ showT amount <> suffix)
    . over playCode (action <>)
    $ template

addPlayEffect = do
  env <- get

  effect <- ActionEval mempty <$> argAt 0
  template <- argAt 1

  return . toUConst $ over playCode (effect <>) template

addPlayGuard = do
  env <- get

  guardF   <- argAt 0
  template <- argAt 1

  case fromU guardF of
    Right expr -> return . toUConst $ set playGuard expr template
    Left x     -> throwError x

addDiscardedEffect = do
  env <- get

  guardF   <- argAt 0
  template <- argAt 1

  case fromU guardF of
    Right expr -> return . toUConst $ set discardEffect expr template
    Left x     -> throwError x

addWoundEffect = do
  env <- get

  f        <- argAt 0
  template <- argAt 1

  case fromU f of
    Right expr -> return . toUConst $ set woundEffect expr template
    Left x     -> throwError x

addFightEffect = do
  env <- get

  label    <- argAt 0
  effect   <- argAt 1
  template <- argAt 2

  action <- eval effect

  case fromU action of
    --Right action' -> return . toUConst $ set fightCode (Just $ mkLabeledExpr label action') template
    Right action' -> return . toUConst $ over fightCode (\as -> mkLabeledExpr label action' <| as) template
    Left x        -> throwError x

addTactic = do
  env <- get

  ability    <- argAt 0
  label      <- argAt 1
  effect     <- argAt 2
  mmTemplate <- argAt 3

  let template = MastermindTacticCard
                   { _mmtName = view mmName mmTemplate
                   , _mmtAbilityName = ability
                   , _mmtAttack = view mmAttack mmTemplate
                   , _mmtVP = view mmVP mmTemplate
                   , _mmtFightCode = S.singleton (mkLabeledExpr label effect)
                   }

  modify (over envCards (template <|))
  return . toUConst $ mmTemplate

addMasterStrike = do
  env <- get

  label    <- argAt 0
  effect   <- argAt 1
  template <- argAt 2

  action <- eval effect

  case fromU action of
    Right action' -> return . toUConst $ set mmStrikeCode (mkLabeledExpr label action') template
    Left x        -> throwError x

atEndStep = do
  effect <- argAt 0

  action <- eval effect

  case fromU action of
    Right action' -> return . toUConst $ ActionEndStep action'
    Left x        -> throwError x

concat = do
  es :: [UExpr] <- argAt 0
  vs :: [UValue] <- traverse eval es

  case mapM fromU vs of
    Right vs' -> return . UConst . UList $ mconcat vs'
    Left y    -> throwError $ "Unexpected error in concat: " <> showT y

concurrently = do
  es :: [UExpr] <- argAt 0
  vs :: [UValue] <- traverse eval es

  case traverse fromU vs of
    Right as -> return . toUConst $ ActionConcurrent as
    Left err -> throwError $ "concurrently didn't get an action: " <> showT err

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

  cards <- toList . view (cardsAtLocation loc) <$> currentBoard

  return . UConst . UList . fmap (UConst . USpecificCard) .
    map (uncurry cardById) $ zip (repeat loc) (fmap (view cardId) cards)

villainsAt = do
  loc <- argAt 0

  cards <- view (cardsAtLocation loc) <$> currentBoard

  let villains = filter isVillain $ zip (toList cards) [0..length cards - 1]

  return . UConst . UList . fmap (UConst . USpecificCard . (\(_, y) -> specificCard loc y)) $ villains

  where
    isVillain (card, _) = case view cardTemplate card of
                           EnemyCard{} -> True
                           _           -> False

heroesAt = do
  loc <- argAt 0

  cards <- view (cardsAtLocation loc) <$> currentBoard

  let villains = filter isHero $ zip (toList cards) [0..length cards - 1]

  return . UConst . UList . fmap (UConst . USpecificCard . (\(_, y) -> specificCard loc y)) $ villains

  where
    isHero (card, _) = case view cardTemplate card of
                           HeroCard{} -> True
                           _          -> False

cardAttr :: ToU a => Traversal' Card a -> EvalMonad UExpr
cardAttr lens = do
  sloc <- argAt 0

  attr <- preview (cardAtLocation sloc . cardTemplate . lens) <$> currentBoard

  case attr of
    Just c -> return . toUConst $ c
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)

cardOwner :: EvalMonad UExpr
cardOwner = do
  specificCard :: SpecificCard <- argAt 0

  case cardLocation specificCard of
    (PlayerLocation pid _) -> return . toUConst $ pid
    _ -> return . UConst $ (UError $ "Location is not a player location: " <> showT specificCard)

isBystander = do
  sloc <- argAt 0

  attr <- preview (cardAtLocation sloc . cardTemplate) <$> currentBoard

  case attr of
    Just BystanderCard -> return . toUConst $ True
    Just _ -> return . toUConst $ False
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)

isWound = do
  sloc <- argAt 0

  attr <- preview (cardAtLocation sloc . cardTemplate) <$> currentBoard

  case attr of
    Just WoundCard -> return . toUConst $ True
    Just _ -> return . toUConst $ False
    Nothing -> return . UConst $ (UError $ "No card at location: " <> showT sloc)

mkChooseCard who descM exprM onChooseM onPassM = do
  pid <- who
  desc <- descM
  exprs <- exprM
  onChoose <- onChooseM
  onPass <- onPassM

  if null exprs then
    return . toUConst $ ActionNone
  else do
    from <- traverse eval exprs

    case traverse fromU from of
      Right from' -> return . toUConst $ ActionChooseCard pid desc from' onChoose onPass
      Left y -> throwError y

chooseYesNo mPid mDesc mYes mNo = do
  pid <- mPid

  desc   <- mDesc
  onYes  <- mYes
  onNo   <- mNo

  return . toUConst $ ActionChooseYesNo pid desc onYes onNo

compose = do
  f1 <- argAt 0
  f2 <- argAt 1
  x  <- argAt 2

  return $ UApp f1 (UApp f2 x)

currentBoard = do
  board <- gets $ view envBoard

  case board of
    Nothing -> throwError "Board function called outside of context"
    Just b -> return b

currentPlayer :: EvalMonad PlayerId
currentPlayer = do
  board <- gets $ view envBoard

  case board of
    Nothing -> throwError "Board function called outside of context"
    Just b ->
      case preview (players . element 0 . playerId) b of
        Nothing -> throwError "No current player"
        Just p -> return p

allPlayers = do
  board <- currentBoard
  ps <- toList . view players <$> currentBoard

  return . toUConst . map (view playerId) $ ps

playerDirection dir = do
  pid <- argAt 0

  ps <- view players <$> currentBoard

  let mi = S.findIndexL (\p -> view playerId p == pid) ps

  case mi of
    Nothing -> throwError $ "Not a valid player id: " <> showT pid
    Just i -> do
      let ni = (i + dir + length ps) `mod` length ps

      return . toUConst . view playerId . S.index ps $ ni



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
                  , _playCode = mempty
                  , _playGuard = parseUnsafe "@(fn [x] x)"
                  , _discardEffect = parseUnsafe "@(fn [x] noop)"
                  , _woundEffect = parseUnsafe "@(fn [continue b] continue)"
                  , _recruitPip = mempty
                  , _attackPip = mempty
                  }

  template' <- eval (UApp callback template)

  case fromU template' of
    Right x -> do
      modify (over envCards (x <|))
      upure ()

    Left y -> throwError y

makeHenchmen = do
  name     <- argAt 0
  attack   <- argAt 1
  vp       <- argAt 2
  callback <- argAt 3

  let template = UConst . UCardTemplate $ EnemyCard
                  { _enemyName = name
                  , _enemyTribe = "Henchmen"
                  , _enemyStartingNumber = 10 -- TODO: No idea what this should be
                  , _enemyAttack = mkModifiableInt attack mempty
                  , _enemyVP = mkModifiableInt vp mempty
                  , _enemyDescription = mempty
                  , _fightCode = mempty
                  , _fightGuard = parseUnsafe "@(fn [x] x)"
                  , _escapeCode = mempty
                  , _ambushCode = mempty
                  }

  template' <- eval (UApp callback template)

  case fromU template' of
    Right x -> do
      modify (over envCards (x <|))
      upure ()

    Left y -> throwError y

makeMastermind = do
  name        <- argAt 0
  alwaysLeads <- argAt 1
  attack      <- argAt 2
  vp          <- argAt 3
  callback    <- argAt 4


  let template = UConst . UCardTemplate $ MastermindCard
                   { _mmName = name
                   , _mmAlwaysLeads = alwaysLeads
                   , _mmAttack = mkModifiableInt attack Nothing
                   , _mmVP = mkModifiableInt vp Nothing
                   , _mmStrikeCode = mempty
                   }

  template' <- eval (UApp callback template)

  case fromU template' of
    Right x -> do
      modify (over envCards (x <|))
      upure ()

    Left y -> throwError y

trace = do
  let name = "_a0"

  vars <- currentVars

  case view (at name) vars of
    Nothing -> throwError $ "Not in env: " <> showT name
    Just x -> do
      result <- eval x
      traceM . show $ result
      return . UConst $ result

tail = do
  xs <- argAt 0

  return . UConst . UList $ f xs


  where
    f [] = []
    f (_:xs) = xs

head = do
  xs :: [UExpr] <- argAt 0

  case xs of
    (x:_) -> return x
    _     -> return . UConst $ UError "head called on empty list"

uniq = do
  xs <- argAt 0

  return . UConst . UList $ nub xs
