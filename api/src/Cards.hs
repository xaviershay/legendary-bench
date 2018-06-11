{-# LANGUAGE OverloadedStrings #-}

module Cards (moneyCard, attackCard, villianCard) where

import qualified Data.Sequence as S
import Types
import Utils
import Action
import GameMonad
import Evaluator

import CardLang.Parser

villianCard = EnemyCard
  { _enemyName = "Villain"
  , _baseHealth = 3
  }

moneyCard = HeroCard
  { _heroName = "S.H.E.I.L.D Agent"
  , _heroAbilityName = mempty
  , _heroTeam = HeroTeam "S.H.E.I.L.D"
  , _heroType = mempty
  , _heroDescription = mempty
  , _playEffect = ActionNone
  , _playCode = parseUnsafe "@(recruit 1)"
  , _heroCost = 0
  , _heroStartingNumber = 0
  }

attackCard = HeroCard
  { _heroName = "S.H.E.I.L.D Trooper"
  , _heroAbilityName = mempty
  , _heroTeam = HeroTeam "S.H.E.I.L.D"
  , _heroType = mempty
  , _heroDescription = mempty
  , _playEffect = ActionNone
  , _playCode = parseUnsafe "@(attack 1)"
  , _heroCost = 0
  , _heroStartingNumber = 0
  }
