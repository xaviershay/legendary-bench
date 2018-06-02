{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CardLang.Parser (parse) where

import           Control.Applicative     ((<|>))
import           Data.SCargot
import           Data.SCargot.Comments   (withLispComments)
import           Data.SCargot.Repr.Basic
import qualified Data.Text               as T
import           Text.Parsec             (alphaNum, between, char, digit, many,
                                          many1, noneOf, oneOf)
import           Text.Parsec.Text        (Parser)

import CardLang.Types
import Utils

data Atom = AInt Int | AString T.Text | ASymbol T.Text deriving (Show, Eq)

parse :: T.Text -> Either String UExpr
parse x = USequence <$> decode myParser x

pAtom :: Parser Atom
pAtom =  parseInt
     <|> parseSymbol
     <|> parseString

parseInt = ((AInt . read) <$> many1 digit)
parseSymbol = ((ASymbol . T.pack) <$> many1 (alphaNum <|> char '-'))
parseString = AString . T.pack <$> quotedString

quotedString = do
  string <- between (char '"') (char '"') (many quotedStringChar)
  return string
  where
    quotedStringChar = escapedChar <|> normalChar
    escapedChar = (char '\\') *> (oneOf ['\\', '"'])
    normalChar = noneOf "\""

vec p = do
  atoms <- vec' p

  pure $ SCons (A (ASymbol "list")) atoms

vec' p = do
  atoms <- (char ']' *> pure SNil) <|> (SCons <$> p <*> vec' p)

  return atoms

myParser = withLispComments $ addReader '[' vec $ setCarrier toExpr $ mkParser pAtom

toExpr :: SExpr Atom -> Either String UExpr
toExpr (A (AInt x)) = Right . UConst . UInt . Sum $ x
toExpr (A (AString x)) = Right . UConst . UString $ x
toExpr (A (ASymbol "let") ::: (A (ASymbol "list") ::: L ls) ::: f ::: Nil) = convertLet ls f
toExpr (A (ASymbol "fn") ::: L vs ::: f ::: Nil) = convertFn vs f
toExpr (A (ASymbol "defn") ::: (A (ASymbol name)) ::: (A (ASymbol "list") ::: L vs) ::: f ::: Nil) = do
  fn <- convertFn vs f
  return $ UDef name fn
toExpr (A (ASymbol "def") ::: (A (ASymbol name)) ::: f ::: Nil) = do
  body <- toExpr f
  return $ UDef name body

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

convertFn :: [SExpr Atom] -> SExpr Atom -> Either String UExpr
convertFn [] f = toExpr f
convertFn (A (ASymbol x):xs) f = do
  body <- convertFn xs f

  return . UConst $ UFunc mempty x body

convertApp [A (ASymbol x)] = return . UVar $ x
convertApp [a] = toExpr a
convertApp (a:rest) = do
  xs <- convertApp rest
  a' <- toExpr a

  return $ UApp xs a'

convertList :: [SExpr Atom] -> Either String [UExpr]
convertList [] = return []
convertList (a:as) = do
  a' <- toExpr a
  as' <- convertList as

  return $ a':as'
