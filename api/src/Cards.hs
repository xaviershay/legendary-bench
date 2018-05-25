{-# LANGUAGE OverloadedStrings #-}

module Cards where

import Types
import Utils
import Action
import GameMonad
import Evaluator

villianCard = EnemyCard
  { _enemyName = "Villian"
  , _baseHealth = 3
  }

moneyCard = HeroCard
  { _heroName = "Money"
  , _playEffect = EffectMoney 1
  , _heroCost = 0
  }

attackCard = HeroCard
  { _heroName = "Attack"
  , _playEffect = EffectAttack 1
  , _heroCost = 0
  }

spideyCard = HeroCard
  { _heroName = "Spiderman"
  , _playEffect =    EffectMoney 1
                  <> EffectCustom "Reveal top card of deck, if cost ≤ 2 then draw it." spideyAction
  , _heroCost = 2
  }

  where
    spideyAction :: GameMonad Action
    spideyAction = do
      playerId <- currentPlayer

      let location = (PlayerLocation playerId PlayerDeck, 0)

      card <- lookupCard location

      return $
           RevealCard location All
        <> ActionIf
             (ConditionCostLTE location 2)
             (drawAction 1 playerId)
             mempty
