{-# LANGUAGE OverloadedStrings #-}

module Cards (villianCard) where

import qualified Data.Sequence as S
import Types
import Utils
import Action
import GameMonad
import Evaluator

import CardLang.Parser

villianCard = EnemyCard
  { _enemyName = "Villain"
  , _enemyAttack = mkModifiableInt 3 Nothing
  }
