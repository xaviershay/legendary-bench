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
import           CardLang.Evaluator     (argAt, toUConst, upure, uliftA1, uliftA2, uliftA3)
import qualified CardLang.Evaluator
import qualified CardLang.Parser
import qualified CardLang.TypeInference
import qualified Data.HashMap.Strict    as M

import Utils
import Types

evalWith :: UEnv -> UExpr -> UValue
evalWith = CardLang.Evaluator.evalWith

evalCards = CardLang.Evaluator.evalCards (mkEnv Nothing)
typecheck = CardLang.TypeInference.typecheck

parse = CardLang.Parser.parse

mkEnv :: Maybe Board -> UEnv
mkEnv board =
  let builtIns = defaultBuiltIns in
    set envBoard board
  $ set envBuiltIn (M.map genBuiltInExpr builtIns)
  $ set envBuiltInDefs builtIns
    emptyEnv

genBuiltInExpr :: BuiltInDef -> UExpr
genBuiltInExpr bi = typeToFn 0 bi mtype
  where
    (Forall _ mtype) = view builtInType bi

    typeToFn :: Int -> BuiltInDef -> MType -> UExpr
    typeToFn n def (WFun _ b) = UConst . UFunc
      $ UFuncData
          { _fnBindings = mempty
          , _fnArgName  = "_a" <> showT n
          , _fnBody     = typeToFn (n+1) def b
          , _fnFreeVars = mempty
          }
    typeToFn n def _ = UBuiltIn (view builtInName def)

defaultBuiltIns = M.fromList . fmap (\x -> (view builtInName x, x)) $
  [ mkBuiltIn "add" ("Int" ~> "Int" ~> "Int") $ B.binOp ((+) :: Int -> Int -> Int)
  , mkBuiltIn "+" ("Int" ~> "Int" ~> "Int") $ B.binOp ((+) :: Int -> Int -> Int)
  , mkBuiltIn "-" ("Int" ~> "Int" ~> "Int") $ B.binOp ((-) :: Int -> Int -> Int)
  , mkBuiltIn "*" ("Int" ~> "Int" ~> "Int") $ B.binOp ((*) :: Int -> Int -> Int)
  , mkBuiltIn "mod" ("Int" ~> "Int" ~> "Int") $ B.binOp (mod :: Int -> Int -> Int)
  , mkBuiltIn "<=" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((<=) :: Int -> Int -> Bool)
  , mkBuiltIn ">=" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((>=) :: Int -> Int -> Bool)
  , mkBuiltIn "<" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((<) :: Int -> Int -> Bool)
  , mkBuiltIn ">" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((>) :: Int -> Int -> Bool)
  , mkBuiltIn "==" ("a" ~> "a" ~> "Bool") $ B.binOp ((==) :: UValue -> UValue -> Bool)
  , mkBuiltIn "and" ("Bool" ~> "Bool" ~> "Bool") $ B.binOp (&&)
  , mkBuiltIn "not" ("Bool" ~> "Bool") $ toUConst . not <$> (argAt 0)
  , mkBuiltIn "reduce" (("b" ~> "a" ~> "b") ~> "b" ~> WList "a" ~> "b") B.reduce
  , mkBuiltIn "concat" (WList (WList "x") ~> WList "x") B.concat
  , mkBuiltIn "combine" ("Action" ~> "Action" ~> "Action") $ B.binOp ((<>) :: Action -> Action -> Action)
  , mkBuiltIn "compose" (("b" ~> "c") ~> ("a" ~> "b") ~> ("a" ~> "c")) B.compose
  , mkBuiltIn "head" (WList "a" ~> "a") B.head
  , mkBuiltIn "tail" (WList "a" ~> WList "a") B.tail
  , mkBuiltIn "uniq" (WList "a" ~> WList "a") B.uniq

  --- Action generators
  , mkBuiltIn "noop" "Action" $ upure ActionNone
  , mkBuiltIn "add-turn" ("PlayerId" ~> "Action")    $ uliftA1 ActionAddTurn (argAt 0)
  , mkBuiltIn "attack" ("Int" ~> "Action")           $ uliftA2 ActionAttack B.currentPlayer (argAt 0)
  , mkBuiltIn "recruit" ("Int" ~> "Action")          $ uliftA2 ActionRecruit B.currentPlayer (argAt 0)
  , mkBuiltIn "rescue-bystander" ("Int" ~> "Action") $ uliftA2 ActionRescueBystander B.currentPlayer (argAt 0)
  , mkBuiltIn "capture-bystander" ("SpecificCard" ~> "Int" ~> "Action") $ uliftA2 ActionCaptureBystander (argAt 0) (argAt 1)
  , mkBuiltIn "draw" ("Int" ~> "Action")             $ uliftA2 ActionDraw B.currentPlayer (argAt 0)
  , mkBuiltIn "reveal" ("SpecificCard" ~> "Action")           $ uliftA2 ActionVisibility (argAt 0) (pure All)
  , mkBuiltIn "reveal-to-owner" ("SpecificCard" ~> "Action")  $ uliftA2 ActionVisibility (argAt 0) (pure Owner)
  , mkBuiltIn "hide" ("SpecificCard" ~> "Action")             $ uliftA2 ActionVisibility (argAt 0) (pure Hidden)
  , mkBuiltIn "ko" ("SpecificCard" ~> "Action")      $ uliftA1 ActionKO (argAt 0)
  , mkBuiltIn "gain" ("SpecificCard" ~> "Action")    $ uliftA2 ActionGain B.currentPlayer (argAt 0)
  , mkBuiltIn "gain-wound-to" ("Location" ~> "Int" ~> "Action") $ uliftA3 ActionGainWound B.currentPlayer (argAt 0) (argAt 1)
  , mkBuiltIn "player-gain-wound" ("PlayerId" ~> "Int" ~> "Action") $ uliftA3 ActionGainWound (argAt 0) (PlayerLocation <$> argAt 0 <*> pure Discard) (argAt 1)
  , mkBuiltIn "discard" ("SpecificCard" ~> "Action") $ uliftA1 ActionDiscardCard (argAt 0)
  , mkBuiltIn "defeat" ("SpecificCard" ~> "Action")  $ uliftA2 ActionDefeat B.currentPlayer (argAt 0)
  , mkBuiltIn "move" ("SpecificCard" ~> "Location" ~> "Action")
    $ uliftA3 ActionMove (argAt 0) (argAt 1) (pure Back)
  , mkBuiltIn "move-top" ("SpecificCard" ~> "Location" ~> "Action")
    $ uliftA3 ActionMove (argAt 0) (argAt 1) (pure Front)
  , mkBuiltIn "draw-player" ("PlayerId" ~> "Int" ~> "Action") $ uliftA2 ActionDraw (argAt 0) (argAt 1)
  , mkBuiltIn "concurrently" (WList "Action" ~> "Action") B.concurrently

  -- Card functions
  , mkBuiltIn "is-bystander" ("SpecificCard" ~> "Bool")   B.isBystander
  , mkBuiltIn "is-wound" ("SpecificCard" ~> "Bool")       B.isWound
  , mkBuiltIn "card-cost" ("SpecificCard" ~> "Int")       $ B.cardAttr heroCost
  , mkBuiltIn "card-type" ("SpecificCard" ~> "String")    $ B.cardAttr heroType
  , mkBuiltIn "card-team" ("SpecificCard" ~> "String")    $ B.cardAttr heroTeam
  , mkBuiltIn "card-owner" ("SpecificCard" ~> "PlayerId") B.cardOwner
  , mkBuiltIn "location" ("String" ~> "Location") $ argAt 0
  , mkBuiltIn "card-location" ("Location" ~> "Int" ~> "SpecificCard") $ uliftA2 specificCard (argAt 0) (argAt 1)
  , mkBuiltIn "cards-at" ("Location" ~> WList "SpecificCard") B.cardsAt
  , mkBuiltIn "city-locations" (WList "Location") (pure . UConst . UList $ fmap toUConst allCityLocations)
  , mkBuiltIn "villains-at" ("Location" ~> WList "SpecificCard") B.villainsAt
  , mkBuiltIn "heroes-at" ("Location" ~> WList "SpecificCard") B.heroesAt
  , mkBuiltIn "player-location" ("PlayerId" ~> "String" ~> "Location")
    $ uliftA2 PlayerLocation (argAt 0) (argAt 1)
  , mkBuiltIn "choose-card"
      (  "String"
      ~> WList "SpecificCard"
      ~> ("SpecificCard" ~> "Action")
      ~> "Action"
      ~> "Action"
      )
      $ B.mkChooseCard B.currentPlayer (argAt 0) (argAt 1) (argAt 2) (Just <$> argAt 3)
  , mkBuiltIn "player-choose-card"
      (  "PlayerId"
      ~> "String"
      ~> WList "SpecificCard"
      ~> ("SpecificCard" ~> "Action")
      ~> "Action"
      ~> "Action"
      )
      $ B.mkChooseCard (argAt 0) (argAt 1) (argAt 2) (argAt 3) (Just <$> argAt 4)
  , mkBuiltIn "must-choose-card"
      (  "String"
      ~> WList "SpecificCard"
      ~> ("SpecificCard" ~> "Action")
      ~> "Action"
      )
      $ B.mkChooseCard B.currentPlayer (argAt 0) (argAt 1) (argAt 2) (pure Nothing)
  , mkBuiltIn "player-must-choose-card"
      (  "PlayerId"
      ~> "String"
      ~> WList "SpecificCard"
      ~> ("SpecificCard" ~> "Action")
      ~> "Action"
      )
      $ B.mkChooseCard (argAt 0) (argAt 1) (argAt 2) (argAt 3) (pure Nothing)
  , mkBuiltIn "choose-yesno"
      (  "String"
      ~> "Action"
      ~> "Action"
      ~> "Action"
      )
      $ B.chooseYesNo B.currentPlayer (argAt 0) (argAt 1) (argAt 2)
  , mkBuiltIn "player-choose-yesno"
      (  "PlayerId"
      ~> "String"
      ~> "Action"
      ~> "Action"
      ~> "Action"
      )
      $ B.chooseYesNo (argAt 0) (argAt 1) (argAt 2) (argAt 3)
  , mkBuiltIn "must-choose"
      (  (WList (WTuple "String" "Action"))
      ~> "Action"
      )
      $ B.mustChoose B.currentPlayer (argAt 0)

  -- Misc
  , mkBuiltIn "tuple" ("a" ~> "b" ~> WTuple "a" "b") $ uliftA2 (,) (argAt 0 :: EvalMonad UExpr) (argAt 1 :: EvalMonad UExpr)
  , mkBuiltIn "current-player" "PlayerId" $ toUConst <$> B.currentPlayer
  , mkBuiltIn "current-card" "SpecificCard" (error "current-card called when not in context")
  , mkBuiltIn "trace" ("a" ~> "a") B.trace
  , mkBuiltIn "player-left" ("PlayerId" ~> "PlayerId") $ B.playerDirection (-1)
  , mkBuiltIn "player-right" ("PlayerId" ~> "PlayerId") $ B.playerDirection 1
  , mkBuiltIn "all-players" (WList "PlayerId") B.allPlayers
  , mkBuiltIn "at-end-step" ("Action" ~> "Action") B.atEndStep
  , mkBuiltIn "add-recruit" ("Int" ~> "CardTemplate" ~> "CardTemplate") $ B.addPip recruitPip "recruit" mempty
  , mkBuiltIn "add-recruit-plus" ("Int" ~> "CardTemplate" ~> "CardTemplate")$ B.addPip recruitPip "recruit" "+"
  , mkBuiltIn "add-attack" ("Int" ~> "CardTemplate" ~> "CardTemplate") $ B.addPip attackPip "attack" mempty
  , mkBuiltIn "add-attack-plus" ("Int" ~> "CardTemplate" ~> "CardTemplate") $ B.addPip attackPip "attack" "+"
  , mkBuiltIn "add-play-effect" (WBoardF "Action" ~> "CardTemplate" ~> "CardTemplate") B.addPlayEffect
  , mkBuiltIn "add-play-guard" (WBoardF ("Action" ~> "Action") ~> "CardTemplate" ~> "CardTemplate") B.addPlayGuard
  , mkBuiltIn "add-discarded-effect" (WBoardF ("SpecificCard" ~> "Action") ~> "CardTemplate" ~> "CardTemplate") B.addDiscardedEffect
  , mkBuiltIn "add-wound-effect" (WBoardF
     (  "Action"
     ~> "SpecificCard"
     ~> "Action"
     ) ~> "CardTemplate" ~> "CardTemplate")
     B.addWoundEffect
  , mkBuiltIn "add-fight-effect" ("String" ~> WBoardF "Action" ~> "CardTemplate" ~> "CardTemplate") B.addFightEffect
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
  , mkBuiltIn "make-mastermind"
    (  "String"
    ~> "String"
    ~> "Int"
    ~> "Int"
    ~> ("CardTemplate" ~> "CardTemplate")
    ~> "Void"
    )
    B.makeMastermind
  , mkBuiltIn "add-master-strike" ("String" ~> WBoardF "Action" ~> "CardTemplate" ~> "CardTemplate") B.addMasterStrike
  , mkBuiltIn "add-tactic" ("String" ~> "String" ~> WBoardF "Action" ~> "CardTemplate" ~> "CardTemplate") B.addTactic
  , mkBuiltIn "make-henchmen"
     (  "String"
     ~> "Int"
     ~> "Int"
     ~> ("CardTemplate" ~> "CardTemplate")
     ~> "Void"
     )
     B.makeHenchmen
  ]
  where
    mkBuiltIn name t f = BuiltInDef
      { _builtInName = name
      , _builtInType = ptype
      , _builtInFn = f
      }
      where
        ptype = CardLang.TypeInference.generalize mempty t
