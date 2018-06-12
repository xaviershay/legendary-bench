{-# LANGUAGE OverloadedStrings #-}

module CardLang
  ( evalWith
  , evalCards
  , typecheck
  , parse
  , mkEnv
  )
  where

import qualified CardLang.BuiltIn       as B
import           CardLang.Evaluator     (FromU, ToU, argAt, toU, toUConst, upure, uliftA1, uliftA2, uliftA3)
import qualified CardLang.Evaluator
import qualified CardLang.Parser
import qualified CardLang.TypeInference
import           Control.Applicative    (liftA2, liftA3)
import           Control.Lens           (element, preview)
import           Control.Monad.Except   (throwError)
import           Control.Monad.State    (get)
import qualified Data.HashMap.Strict    as M
import qualified Data.Sequence          as S

import CardLang.Types

import Utils
import Types

evalWith :: UEnv -> UExpr -> UValue
evalWith env = CardLang.Evaluator.evalWith env

evalCards = CardLang.Evaluator.evalCards (mkEnv Nothing)
typecheck = CardLang.TypeInference.typecheck

parse = CardLang.Parser.parse

mkEnv :: Maybe Board -> UEnv
mkEnv board =
  let builtIns = defaultBuiltIns in
    set envBoard board
  $ set envBuiltIn (M.map genBuiltInExpr builtIns)
  $ set envBuiltInDefs builtIns
  $ emptyEnv

genBuiltInExpr :: BuiltInDef -> UExpr
genBuiltInExpr bi = typeToFn 0 bi mtype
  where
    (Forall _ mtype) = view builtInType bi

    typeToFn :: Int -> BuiltInDef -> MType -> UExpr
    typeToFn n def (WFun _ b) = UConst . UFunc
      $ UFuncData
          { _fnBindings = mempty
          , _fnArgName  = ("_a" <> showT n)
          , _fnBody     = typeToFn (n+1) def b
          , _fnFreeVars = mempty
          }
    typeToFn n def _ = UBuiltIn (view builtInName def)

defaultBuiltIns = M.fromList . fmap (\x -> (view builtInName x, x)) $
  [ mkBuiltIn "add" ("Int" ~> "Int" ~> "Int") $ B.binOp ((+) :: Int -> Int -> Int)
  , mkBuiltIn "+" ("Int" ~> "Int" ~> "Int") $ B.binOp ((+) :: Int -> Int -> Int)
  , mkBuiltIn "-" ("Int" ~> "Int" ~> "Int") $ B.binOp ((-) :: Int -> Int -> Int)
  , mkBuiltIn "<=" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((<=) :: Int -> Int -> Bool)
  , mkBuiltIn ">=" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((>=) :: Int -> Int -> Bool)
  , mkBuiltIn "<" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((<) :: Int -> Int -> Bool)
  , mkBuiltIn ">" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((>) :: Int -> Int -> Bool)
  , mkBuiltIn "==" ("a" ~> "a" ~> "Bool") $ B.binOp ((==) :: UValue -> UValue -> Bool)
  , mkBuiltIn "reduce" (("b" ~> "a" ~> "b") ~> "b" ~> WList "a" ~> "b")  B.reduce
  , mkBuiltIn "concat" (WList (WList "x") ~> WList "x") B.concat
  , mkBuiltIn "combine" ("Action" ~> "Action" ~> "Action") $ B.binOp ((<>) :: Action -> Action -> Action)
  , mkBuiltIn "compose" (("b" ~> "c") ~> ("a" ~> "b") ~> ("a" ~> "c")) $ B.compose
  , mkBuiltIn "tail" (WList "a" ~> WList "a") $ B.tail

  --- Action generators
  , mkBuiltIn "noop" "Action" $ upure ActionNone
  , mkBuiltIn "attack" ("Int" ~> "Action")           $ uliftA2 ActionAttack B.currentPlayer (argAt 0)
  , mkBuiltIn "recruit" ("Int" ~> "Action")          $ uliftA2 ActionRecruit B.currentPlayer (argAt 0)
  , mkBuiltIn "rescue-bystander" ("Int" ~> "Action") $ uliftA2 ActionRescueBystander B.currentPlayer (argAt 0)
  , mkBuiltIn "draw" ("Int" ~> "Action")             $ uliftA2 ActionDraw B.currentPlayer (argAt 0)
  , mkBuiltIn "reveal" ("SpecificCard" ~> "Action")  $ uliftA1 ActionReveal (argAt 0)
  , mkBuiltIn "ko" ("SpecificCard" ~> "Action")      $ uliftA1 ActionKO (argAt 0)
  , mkBuiltIn "discard" ("SpecificCard" ~> "Action") $ uliftA1 ActionDiscardCard (argAt 0)
  , mkBuiltIn "move" ("SpecificCard" ~> "Location" ~> "Action")
    $ uliftA3 ActionMove (argAt 0) (argAt 1) (pure Back)

  -- Card functions
  , mkBuiltIn "card-cost" ("SpecificCard" ~> "Int")    $ B.cardAttr heroCost
  , mkBuiltIn "card-type" ("SpecificCard" ~> "String") $ B.cardAttr heroType
  , mkBuiltIn "card-team" ("SpecificCard" ~> "String") $ B.cardAttr heroTeam
  , mkBuiltIn "card-owner" ("SpecificCard" ~> "PlayerId") $ B.cardOwner
  , mkBuiltIn "card-location" ("Location" ~> "Int" ~> "SpecificCard") $ uliftA2 specificCard (argAt 0) (argAt 1)
  , mkBuiltIn "cards-at" ("Location" ~> WList "SpecificCard") B.cardsAt
  , mkBuiltIn "player-location" ("PlayerId" ~> "String" ~> "Location")
    $ uliftA2 PlayerLocation (argAt 0) (argAt 1)
  , mkBuiltIn "choose-card"
      (  "String"
      ~> WList "SpecificCard"
      ~> ("SpecificCard" ~> "Action")
      ~> "Action"
      ~> "Action"
      )
      B.chooseCard
  , mkBuiltIn "choose-yesno"
      (  "String"
      ~> "Action"
      ~> "Action"
      ~> "Action"
      )
      B.chooseYesNo

  -- Misc
  , mkBuiltIn "current-player" "PlayerId" $ toUConst <$> B.currentPlayer
  , mkBuiltIn "add-play-effect" (WBoardF "Action" ~> "CardTemplate" ~> "CardTemplate") B.addPlayEffect
  , mkBuiltIn "add-play-guard" (WBoardF ("Action" ~> "Action") ~> "CardTemplate" ~> "CardTemplate") B.addPlayGuard
  , mkBuiltIn "add-discarded-effect" (WBoardF ("SpecificCard" ~> "Action") ~> "CardTemplate" ~> "CardTemplate") B.addDiscardedEffect
  , mkBuiltIn "make-hero-full"
     (  "String"
     ~> "String"
     ~> "String"
     ~> "String"
     ~> "Int"
     ~> "Int"
     ~> "String"
     ~> ("CardTemplate" ~> "CardTemplate")
     ~> "Void"
     )
     B.makeHero
  ]
  where
    mkBuiltIn name t f = BuiltInDef
      { _builtInName = name
      , _builtInType = ptype
      , _builtInFn = f
      }
      where
        ptype = CardLang.TypeInference.generalize mempty t
