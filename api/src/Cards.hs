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
  , _cost = 0
  }

attackCard = HeroCard
  { _heroName = "Attack"
  , _playEffect = EffectAttack 1
  , _cost = 0
  }

spideyCard = HeroCard
  { _heroName = "Spiderman"
  , _playEffect =    EffectMoney 1
                  <> EffectCustom "Reveal top card of deck, if costs less than two then draw it" spideyAction
  , _cost = 2
  }

  where
    spideyAction :: GameMonad Action
    spideyAction = do
      playerId <- currentPlayer

      let location = (PlayerLocation playerId PlayerDeck, 0)

      card <- lookupCard location

      return $ ActionSequence
                 (RevealCard location All)
                 (do
                   (CardInPlay card _) <- requireCard location

                   return $ if cardCost card <= 2 then
                              drawAction 1 playerId
                            else
                              ActionNone
                 )
