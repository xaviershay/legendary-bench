{-# LANGUAGE OverloadedStrings #-}

module QueryTest where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

import Data.SCargot
import Data.SCargot.Repr.Basic
import Control.Applicative ((<|>))
import Text.Parsec (anyChar, char, digit, many1, manyTill, newline, satisfy, string, alphaNum)
import Text.Parsec.Text (Parser)

import Types
import Evaluator
import GameMonad
import Utils

runQuery :: Show a => Term a -> a
runQuery q =
  let state = GameMonadState { _board = mkBoard } in
  let (Right result, log) = runIdentity $ runWriterT (runReaderT (runExceptT $ query q) state) in

  result

data UExpr =
    UConst UValue
  | UVar T.Text
  | ULet (T.Text, UExpr) UExpr
  deriving (Eq, Show)

type UEnv = M.HashMap T.Text UExpr

data Atom = AInt Int | ASymbol T.Text deriving (Show, Eq)

data UValue =
   UNone
 | ULocation Location
 | UInt SummableInt
 | UBool Bool
 | UFunc UExpr

 deriving (Eq, Show)

pAtom :: Parser Atom
pAtom =  ((AInt . read) <$> many1 digit)
     <|> ((ASymbol . T.pack) <$> many1 alphaNum)

uQuery :: UEnv -> UExpr -> UValue
uQuery env (UConst v) = v
uQuery env (UVar label) = uQuery env $ M.lookupDefault (UConst UNone) label env
uQuery env (ULet (key, value) expr) = uQuery (M.insert key value env) expr

convertLet [] f = toExpr f
convertLet (A (ASymbol k):v:vs) f = do
  value <- toExpr v
  body <- convertLet vs f

  return $ ULet (k, value) body
convertLet vs f = fail $ "Invalid let params: " <> show vs

toExpr :: SExpr Atom -> Either String UExpr
toExpr (A (AInt x)) = Right . UConst . UInt . Sum $ x
toExpr (A (ASymbol "let") ::: L ls ::: f ::: Nil) = convertLet ls f
toExpr (A (ASymbol x)) = Right . UVar $ x
toExpr (L _) = return $ UConst UNone

myParser = setCarrier toExpr $ mkParser pAtom

lQuery :: UEnv -> T.Text -> UValue
lQuery env text = case decode myParser text of
                    Right x -> uQuery env (head x)
                    Left y -> error $ show y

test_ListQuery = testGroup "List Query"
  [ testCase "UInt" $ UInt 1 @=? lQuery mempty "1"
  , testCase "UVar None" $ UNone @=? lQuery mempty "x"
  , testCase "UVar Just" $ (UInt 1) @=? lQuery (M.fromList [("x", UConst $ UInt 1)]) "x"
  , testCase "ULet" $ (UInt 1) @=? lQuery (M.fromList [("x", UConst $ UInt 0)]) "(let (x 1) x)"
  , testCase "ULet" $ (UInt 2) @=? lQuery mempty "(let (x 1 y 2) y)"
  ]

focus = defaultMain test_ListQuery
