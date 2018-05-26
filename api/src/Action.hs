{-# LANGUAGE OverloadedStrings #-}

module Action where

import Types
import Utils
import GameMonad
import Control.Lens

-- Convert card effects to actions
-- ===============================
playAction :: CardInPlay -> GameMonad Action
playAction = effectAction . view (cardTemplate . playEffect)

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

replaceHeroInHQ i = ActionTagged ("Replace hero in spot " <> showT i) $
  revealAndMove (HeroDeck, 0) HQ (LocationIndex i)

revealAndMove source destination spot =
     RevealCard source All
  <> MoveCard source destination spot

drawAction :: Int -> PlayerId -> Action
drawAction n pid = ActionTagged (playerDesc pid <> " draws " <> showT n) $
   mconcat . replicate n $
     revealAndMove
       (PlayerLocation pid PlayerDeck, 0)
       (PlayerLocation pid Hand)
       Front
