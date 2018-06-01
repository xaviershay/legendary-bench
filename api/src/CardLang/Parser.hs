{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CardLang.Parser where

import Data.SCargot
import Data.SCargot.Repr.Basic
import Control.Applicative ((<|>))
import Text.Parsec (anyChar, char, digit, many1, manyTill, newline, satisfy, string, alphaNum)
import Text.Parsec.Text (Parser)
import qualified Data.Text            as T

import CardLang.Types
import Utils

data Atom = AInt Int | ASymbol T.Text deriving (Show, Eq)

parse :: T.Text -> Either String UExpr
parse x = USequence <$> decode myParser x

pAtom :: Parser Atom
pAtom =  ((AInt . read) <$> many1 digit)
     <|> ((ASymbol . T.pack) <$> many1 alphaNum)

vec p = do
  atoms <- vec' p

  pure $ SCons (A (ASymbol "list")) atoms

vec' p = do
  atoms <- (char ']' *> pure SNil) <|> (SCons <$> p <*> vec' p)

  return atoms

myParser = addReader '[' vec $ setCarrier toExpr $ mkParser pAtom

toExpr :: SExpr Atom -> Either String UExpr
toExpr (A (AInt x)) = Right . UConst . UInt . Sum $ x
toExpr (A (ASymbol "let") ::: (A (ASymbol "list") ::: L ls) ::: f ::: Nil) = convertLet ls f
toExpr (A (ASymbol "fn") ::: L vs ::: f ::: Nil) = convertFn vs f
toExpr (A (ASymbol "list") ::: L rest) = UConst . UList <$> convertList rest
toExpr (A (ASymbol x)) = Right . UVar $ x
toExpr (L args) = convertApp (reverse args)
toExpr (L _) = return $ UConst UNone
convertLet :: [SExpr Atom] -> SExpr Atom -> Either String UExpr
convertLet [] f = toExpr f
convertLet (A (ASymbol k):v:vs) f = do
  value <- toExpr v
  body <- convertLet vs f

  return $ ULet (k, value) body
convertLet vs f = fail $ "Invalid let params: " <> show vs


-- TODO: Something something env capture
convertFn :: [SExpr Atom] -> SExpr Atom -> Either String UExpr
convertFn [] f = toExpr f
convertFn (A (ASymbol x):xs) f = do
  body <- convertFn xs f

  return . UConst $ UFunc mempty x body

-- TODO: Handle expr in first element
convertApp [A (ASymbol x)] = return . UVar $ x
convertApp [a] = toExpr a
convertApp (a:rest) = do
  xs <- convertApp rest
  a'   <- toExpr a

  return $ UApp xs a'

convertList :: [SExpr Atom] -> Either String [UExpr]
convertList [] = return []
convertList (a:as) = do
  a' <- toExpr a
  as' <- convertList as

  return $ a':as'
