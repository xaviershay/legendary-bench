module Props where

import           Control.Lens          (over, set, view)
import           Control.Monad         (liftM, liftM2, liftM3)
import           Data.Hashable         (Hashable)
import qualified Data.HashMap.Strict   as M
import qualified Data.Sequence         as S
import qualified Data.Text             as T
import           Evaluator
import           GameMonad
import           System.Random         (mkStdGen)
import           Test.Tasty.QuickCheck

import Types

instance Arbitrary Board where
  shrink board =
    let existingCards = M.toList . view cards $ board in
    map
      (\xs -> set cards (M.fromList xs) board)
      (shrink existingCards)

  arbitrary = do
    ps <- listOf1 arbitrary
    ls <- listOf $ genLocation ps
    cs <- sequence $ map genCards ls

    let cardMap = setVisibilities $ zip ls cs

    return .
      set cards (M.fromList cardMap)
      $ mkBoard

setVisibilities [] = []
setVisibilities (l:ls) = f l:setVisibilities ls
  where
    f (l, cs) = (l, fmap (\c -> CardInPlay {_cardTemplate = c, _cardVisibility = defaultVisibilityFor l, _cardId = CardId 0}) cs)

defaultVisibilityFor HeroDeck = Hidden
defaultVisibilityFor VillainDeck = Hidden
defaultVisibilityFor (PlayerLocation _ PlayerDeck) = Hidden
defaultVisibilityFor _ = All

instance Arbitrary CardInPlay where
  shrink = genericShrink
  arbitrary = CardInPlay <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary T.Text where
  shrink x = fmap T.pack $ genericShrink (T.unpack x)
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Card where
  shrink = genericShrink
  arbitrary = oneof [genHero, genEnemy]

instance Arbitrary PlayerId where
  shrink = genericShrink
  arbitrary = PlayerId <$> arbitrary

instance Arbitrary HeroTeam where
  shrink = genericShrink
  arbitrary = HeroTeam <$> arbitrary

instance Arbitrary HeroType where
  shrink = genericShrink
  arbitrary = HeroType <$> arbitrary

instance Arbitrary CardId where
  shrink = genericShrink
  arbitrary = CardId <$> arbitrary

instance Arbitrary ScopedLocation where
  shrink = genericShrink
  arbitrary = elements [(minBound :: ScopedLocation)..]

instance Arbitrary Effect where
  shrink EffectNone = []
  shrink _ = [EffectNone]

  arbitrary = sized f
    where
      f 0 = oneof [ pure EffectNone
                  , EffectMoney . Sum <$> elements [0..10]
                  , EffectAttack . Sum <$> elements [0..10]
                  ]
      f n = oneof [f 0, liftM2 EffectCombine sub sub]
        where
          sub = f (n `div` 2)

instance Arbitrary Location where
  shrink = genericShrink
  arbitrary = oneof
              [ pure HQ
              , pure KO
              , pure HeroDeck
              , pure VillainDeck
              , City <$> elements [0..4]
              , pure Escaped
              , pure Boss
              , PlayerLocation <$> arbitrary <*> arbitrary
              ]

instance Arbitrary MoveDestination where
  arbitrary = oneof [ pure Front, LocationIndex <$> arbitrary ]
  shrink = genericShrink

instance Arbitrary SummableInt where
  shrink = genericShrink
  arbitrary = Sum <$> arbitrary

instance Arbitrary Resources where
  shrink = genericShrink
  arbitrary = do
    (Positive attack) <- arbitrary
    (Positive money) <- arbitrary

    return $ Resources { _attack = Sum attack, _money = Sum money }

instance Arbitrary Visibility where
  shrink = genericShrink
  arbitrary = elements [(minBound :: Visibility)..]

instance Arbitrary Condition where
  shrink = genericShrink
  arbitrary = ConditionCostLTE <$> arbitrary <*> arbitrary

instance Arbitrary Action where
  shrink = genericShrink
  arbitrary = sized f
    where
      f 0 = oneof [ pure ActionNone
                  , liftM3 MoveCard arbitrary arbitrary arbitrary
                  ]
      f n = oneof [f 0, liftM2 ActionCombine sub sub]
        where
          sub = f (n `div` 2)

genCards :: Location -> Gen (S.Seq Card)
genCards VillainDeck = S.fromList <$> listOf genEnemy
genCards _ = S.fromList <$> listOf genHero

genHero = do
  name   <- T.pack . getPrintableString <$> arbitrary
  (Positive cost)   <- arbitrary
  effect <- arbitrary

  return $ HeroCard
    { _heroName = name
    , _heroCost = cost
    , _playEffect = effect
    }

genEnemy = do
  name   <- T.pack . getPrintableString <$> arbitrary
  (Positive health) <- arbitrary

  return $ EnemyCard
    { _enemyName = name
    , _baseHealth = health
    }

genLocation ps = oneof
  [ pure HQ
  , pure KO
  , pure HeroDeck
  , pure VillainDeck
  , City <$> elements [0..4]
  , pure Escaped
  , pure Boss
  , PlayerLocation <$> elements ps <*> arbitrary
  ]

prop_totalCardsStaysConstant :: Board -> Action -> Bool
prop_totalCardsStaysConstant board action =
  let board' = runGameMonad board (apply action) in

  check board == check board'

  where
    check = length . mconcat . M.elems . view cards
