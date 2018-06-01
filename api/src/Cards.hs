{-# LANGUAGE OverloadedStrings #-}

module Cards where

import qualified Data.Sequence as S
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
  , _heroDescription = mempty
  , _playEffect = ActionMoney TCurrentPlayer (TConst 1)
  , _heroCost = 0
  }

attackCard = HeroCard
  { _heroName = "S.H.E.I.L.D Trooper"
  , _heroAbilityName = mempty
  , _heroTeam = HeroTeam "S.H.E.I.L.D"
  , _heroType = mempty
  , _heroDescription = mempty
  , _playEffect = ActionAttack TCurrentPlayer (TConst 1)
  , _heroCost = 0
  }

spideyCard = HeroCard
  { _heroName = "Spider-Man"
  , _heroAbilityName = "Astonishing Strength"
  , _heroType = HeroType "Instinct"
  , _heroTeam = HeroTeam "Spider Friends"
  , _playEffect = spiderAction2
  , _heroDescription = mempty
  , _heroCost = 2
  }

spidermanCards =
  [ HeroCard
    { _heroName = "Spider-Man"
    , _heroAbilityName = "Astonishing Strength"
    , _heroType = HeroType "Strength"
    , _heroTeam = HeroTeam "Spider Friends"
    , _playEffect = ActionMoney TCurrentPlayer (TConst 1) <> spiderAction2
    , _heroDescription = "Reveal top card of deck, if cost ≤ 2 then draw it."
    , _heroCost = 2
    }
  , HeroCard
    { _heroName = "Spider-Man"
    , _heroAbilityName = "Great Responsibility"
    , _heroType = HeroType "Instinct"
    , _heroTeam = HeroTeam "Spider Friends"
    , _playEffect = ActionAttack TCurrentPlayer (TConst 1) <> spiderAction2
    , _heroDescription = "Reveal top card of deck, if cost ≤ 2 then draw it."
    , _heroCost = 2
    }
  ]

spiderAction2 = let location = TSpecificCard (TPlayerLocation TCurrentPlayer (TConst PlayerDeck)) (TConst 0) in
  ActionReveal location
  <> ActionIf2
       (TOp (<=) (TCardCost location) (TConst 2))
       (ActionMove location (TPlayerLocation TCurrentPlayer (TConst Hand)) (TConst Front))
       mempty

blackWidowCards =
  [ HeroCard
    { _heroName = "Black Widow"
    , _heroAbilityName = "Dangerous Rescue"
    , _heroType = HeroType "Covert"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 3
    , _heroDescription = "You may KO a card from your hand or discard pile. If you do, rescue a Bystander."
    , _playEffect = ActionAttack TCurrentPlayer (TConst 2) <>
                    ActionOptional
                      (ActionMove
                        (TChooseCard
                          "Choose a card from hand or discard to KO"
                          TCurrentPlayer
                          (mconcat . fmap (TAllCardsAt . TPlayerLocation TCurrentPlayer . TConst) $ [Hand, Discard])
                        )
                        (TConst KO)
                        (TConst Front)
                      )
                      rescueBystander
                      mempty
    }
  , HeroCard
    { _heroName = "Black Widow"
    , _heroAbilityName = "Mission Accomplished"
    , _heroType = HeroType "Tech"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 2
    , _heroDescription = "Draw a card.\n|tech|: Rescue a Bystander."
    , _playEffect = let location = (TSpecificCard (TPlayerLocation TCurrentPlayer (TConst PlayerDeck)) (TConst 0)) in
                        ActionReveal location
                     <> ActionMove
                       location
                       (TPlayerLocation TCurrentPlayer (TConst Hand))
                       (TConst Front)
                     <> ActionIf2
                          (TOp (>) (TPlayedOfType (HeroType "Tech")) (TConst 1))
                          rescueBystander
                          mempty
    }
  , HeroCard
    { _heroName = "Black Widow"
    , _heroAbilityName = "Covert Operation"
    , _heroType = HeroType "Covert"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 4
    , _heroDescription = "You get +1 Attack for each Bystander in your Victory pile."
    , _playEffect = ActionAttack TCurrentPlayer
        (TBystandersAt (TPlayerLocation TCurrentPlayer (TConst Victory)))
    }
  , HeroCard
    { _heroName = "Black Widow"
    , _heroAbilityName = "Silent Sniper"
    , _heroType = HeroType "Covert"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 7
    , _heroDescription = "Defeat a Villian or Mastermind that has a Bystander"
    , _playEffect =    (ActionAttack TCurrentPlayer (TConst 4))
                    <> (ActionFight (TChooseCard
                          "Choose a Villian or Mastermind that has a Bystander"
                          TCurrentPlayer
                          (TMap (\l -> TSpecificCard l (TConst 0))
                            (TFilterBy
                              TBystandersAt
                              (TOp (<) (TConst 0))
                              (TConst $ S.fromList [City 0])))
                        ))
    }
  ]

rescueBystander = (ActionAllowFail $ ActionMove
                    (TSpecificCard (TConst BystanderDeck) (TConst 0))
                    (TPlayerLocation TCurrentPlayer (TConst Victory))
                    (TConst Front)
                  )
