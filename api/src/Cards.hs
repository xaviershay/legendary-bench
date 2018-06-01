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
  , _heroStartingNumber = 0
  }

attackCard = HeroCard
  { _heroName = "S.H.E.I.L.D Trooper"
  , _heroAbilityName = mempty
  , _heroTeam = HeroTeam "S.H.E.I.L.D"
  , _heroType = mempty
  , _heroDescription = mempty
  , _playEffect = ActionAttack TCurrentPlayer (TConst 1)
  , _heroCost = 0
  , _heroStartingNumber = 0
  }

spideyCard = HeroCard
  { _heroName = "Spider-Man"
  , _heroAbilityName = "Astonishing Strength"
  , _heroType = HeroType "Instinct"
  , _heroTeam = HeroTeam "Spider Friends"
  , _playEffect = spiderAction2
  , _heroDescription = mempty
  , _heroCost = 2
  , _heroStartingNumber = 5
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
    , _heroStartingNumber = 5
    }
  , HeroCard
    { _heroName = "Spider-Man"
    , _heroAbilityName = "Great Responsibility"
    , _heroType = HeroType "Instinct"
    , _heroTeam = HeroTeam "Spider Friends"
    , _playEffect = ActionAttack TCurrentPlayer (TConst 1) <> spiderAction2
    , _heroDescription = "Reveal top card of deck, if cost ≤ 2 then draw it."
    , _heroCost = 2
    , _heroStartingNumber = 5
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
    , _heroStartingNumber = 5
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
    , _heroStartingNumber = 5
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
    , _heroStartingNumber = 3
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
    , _heroStartingNumber = 1
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

captainAmericaCards =
  [ HeroCard
    { _heroName = "Captain America"
    , _heroAbilityName = "Avengers Assemble!"
    , _heroType = HeroType "Instinct"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 3
    , _heroStartingNumber = 5
    , _heroDescription = "You get +1 Recruit for each color of Hero you have"
    , _playEffect = ActionMoney TCurrentPlayer (
                      TLength (
                      TUniq (
                      TMap THeroType (
                      (mconcat . fmap (TAllCardsAt . TPlayerLocation TCurrentPlayer . TConst) $ [Hand, Played])))))
    }
  , HeroCard
    { _heroName = "Captain America"
    , _heroAbilityName = "Perfect Teamwork"
    , _heroType = HeroType "Strength"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 4
    , _heroStartingNumber = 5
    , _heroDescription = "You get +1 Attack for each color of Hero you have"
    , _playEffect = ActionAttack TCurrentPlayer (
                      TLength (
                      TUniq (
                      TMap THeroType (
                      (mconcat . fmap (TAllCardsAt . TPlayerLocation TCurrentPlayer . TConst) $ [Hand, Played])))))
    }
  , HeroCard
    { _heroName = "Captain America"
    , _heroAbilityName = "Diving Block"
    , _heroType = HeroType "Tech"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 6
    , _heroStartingNumber = 3
    , _heroDescription = "If you would gain a Would, you may reveal this card and draw a card instead."
    , _playEffect = ActionAttack TCurrentPlayer (TConst 4)
    -- TODO: block effect
    }
  , HeroCard
    { _heroName = "Captain America"
    , _heroAbilityName = "A Day Unlike Any Other"
    , _heroType = HeroType "Covert"
    , _heroTeam = HeroTeam "Avengers"
    , _heroCost = 7
    , _heroStartingNumber = 1
    , _heroDescription = "You get +3 Attack for each other Avengers Hero you played this turn"
    --, _playCode = "fn filterByHeroType(t, cs) { filterBy((x) -> heroType(x) == :avengers, cs) };"
               <> "attack(3);"
               <> "attack(length(filterByHeroType(:avengers, allCardsFrom(:hand, :played)) - 1)"
    , _playEffect = ActionAttack TCurrentPlayer (TConst 3)
                 <> ActionAttack TCurrentPlayer ((TConst (Sum (-1))) <> (
                      TLength $
                      TFilterBy
                        THeroType
                        (TOp (==) (TConst $ HeroType "Avengers"))
                        (mconcat . fmap (TAllCardsAt . TPlayerLocation TCurrentPlayer . TConst) $ [Hand, Played])))

    -- TODO: block effect
    }
  ]
