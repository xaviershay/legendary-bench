{-# LANGUAGE OverloadedStrings #-}

module Cards where

import Types
import Utils
import Action
import GameMonad
import Evaluator

villianCard = EnemyCard
  { _enemyName = "Villain"
  , _baseHealth = 3
  }

moneyCard = HeroCard
  { _heroName = "S.H.E.I.L.D Agent"
  , _heroAbilityName = mempty
  , _heroTeam = HeroTeam "S.H.E.I.L.D"
  , _heroType = mempty
  , _playEffect = ActionMoney QueryCurrentPlayer (QueryConst 1)
  , _heroCost = 0
  }

attackCard = HeroCard
  { _heroName = "S.H.E.I.L.D Trooper"
  , _heroAbilityName = mempty
  , _heroTeam = HeroTeam "S.H.E.I.L.D"
  , _heroType = mempty
  --, _playEffect = EffectAttack (QueryConst 1)
  , _playEffect = ActionNone
  , _heroCost = 0
  }

spideyCard = HeroCard
  { _heroName = "Spider-Man"
  , _heroAbilityName = "Astonishing Strength"
  , _heroType = HeroType "Instinct"
  , _heroTeam = HeroTeam "Spider Friends"
  --, _playEffect =    EffectMoney (QueryConst 1)
  --                <> EffectCustom "Reveal top card of deck, if cost ≤ 2 then draw it." spideyAction
  , _playEffect = ActionNone
  , _heroCost = 2
  }

spidermanCards =
  [ HeroCard
    { _heroName = "Spider-Man"
    , _heroAbilityName = "Astonishing Strength"
    , _heroType = HeroType "Strength"
    , _heroTeam = HeroTeam "Spider Friends"
    , _playEffect = ActionNone
  --  , _playEffect =    EffectMoney (QueryConst 1)
  --                  <> EffectCustom "Reveal top card of deck, if cost ≤ 2 then draw it." spideyAction
    , _heroCost = 2
    }
  , HeroCard
    { _heroName = "Spider-Man"
    , _heroAbilityName = "Great Responsibility"
    , _heroType = HeroType "Instinct"
    , _heroTeam = HeroTeam "Spider Friends"
    , _playEffect = ActionNone
    --, _playEffect =    EffectAttack (QueryConst 1)
    --                <> EffectCustom "Reveal top card of deck, if cost ≤ 2 then draw it." spideyAction
    , _heroCost = 2
    }
  ]


spideyAction :: Action
spideyAction =
  let location = (PlayerLocation CurrentPlayer PlayerDeck, 0) in
  ( RevealCard location All
  <> ActionIf
       (ConditionCostLTE location 2)
       (drawAction 1 CurrentPlayer)
       mempty
  )
