{-# LANGUAGE OverloadedStrings #-}

module Action where

import Types
import Utils
import GameMonad
import Control.Lens

-- Convert player action to intrinsic action
-- =========================================
translatePlayerAction :: PlayerAction -> GameMonad Action
translatePlayerAction (PlayCard i) = do
  playerId <- currentPlayer

  let location = (PlayerLocation playerId Hand, i)

  card <- lookupCard location

  case card of
    Nothing -> return $ ActionLose ("No card at: " <> showT location)
    Just c -> do
      cardEffect <- playAction c

      return $
           revealAndMove location (PlayerLocation playerId Played) Front
        <> cardEffect

translatePlayerAction (PurchaseCard i) = do
  let location = (HQ, i)

  playerId <- currentPlayer
  card <- lookupCard location

  return $ case card of
    Nothing -> ActionLose ("No card to purchase: " <> showT location)
    Just c ->
         MoveCard location (PlayerLocation playerId Discard) Front
      <> ApplyResources playerId (mempty { _money = -(cardCost c)})
      <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)

translatePlayerAction (AttackCard i) = do
  let location = (City i, 0)

  playerId <- currentPlayer
  card <- lookupCard location

  return $ case card of
    Nothing -> ActionLose ("No card to attack: " <> showT location)
    Just c ->
         MoveCard location (PlayerLocation playerId Victory) Front
      <> ApplyResources playerId (mempty { _attack = -(cardHealth c)})

translatePlayerAction EndTurn = return (ActionEndTurn <> ActionStartTurn)

-- Convert card effects to actions
-- ===============================
playAction :: CardInPlay -> GameMonad Action
playAction (CardInPlay card _) = effectAction (view playEffect card)

effectAction :: Effect -> GameMonad Action
effectAction (EffectMoney n) = applyResourcesAction (set money n mempty)
effectAction (EffectAttack n) = applyResourcesAction (set attack n mempty)
effectAction EffectNone = return ActionNone
effectAction (EffectCustom _ f) = f
effectAction (EffectCombine a b) = do
  x <- effectAction a
  y <- effectAction b

  return $ x <> y

applyResourcesAction :: Resources -> GameMonad Action
applyResourcesAction rs = do
  player <- currentPlayer

  return $ ApplyResources player rs

-- Action Builders
-- ===============

revealAndMove source destination spot =
     RevealCard source All
  <> MoveCard source destination spot

drawAction :: PlayerId -> Int -> Action
drawAction playerId n =
   mconcat . replicate n $
     revealAndMove
       (PlayerLocation playerId PlayerDeck, 0)
       (PlayerLocation playerId Hand)
       Front
