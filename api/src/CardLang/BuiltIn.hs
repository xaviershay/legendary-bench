{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardLang.BuiltIn where

import           Control.Monad.Except (throwError)
import           Prelude              (Either (..), Int, Maybe (..), fmap, map,
                                       mapM, mconcat, return, sequence,
                                       traverse, ($), (.), (<$>), (<*>))
import qualified Prelude              as P

import CardLang.Evaluator hiding (argAt)
import Utils
import Types

concat = do
  es :: [UExpr] <- argAt 0
  vs :: [UValue] <- traverse eval es

  case mapM fromU vs of
    Right vs' -> return . UConst . UList $ mconcat vs'
    Left y    -> throwError $ "Unexpected error in concat: " <> showT y

reduce = do
  f       <- argAt 0
  initial <- argAt 1
  xs      <- argAt 2

  foldM (folder f) initial (xs :: [UExpr])

  where
    folder :: UExpr -> UExpr -> UExpr -> EvalMonad UExpr
    folder f accum x = UConst <$> eval (UApp (UApp f accum) x)

binOp :: (FromU a, ToU b) => (a -> a -> b) -> EvalMonad UExpr
binOp f = UConst . toU <$> (f <$> argAt 0 <*> argAt 1)

argAt :: FromU a => Int -> EvalMonad a
argAt index = do
  let name = "_a" <> showT index

  vars <- currentVars

  case view (at name) vars of
                       Nothing -> throwError $ "Not in env: " <> showT name
                       Just x -> do
                         result <- eval x
                         case fromU result of
                           Right x -> return x
                           Left y -> throwError $ "Arg " <> name <> " was not of the right type: " <> showT y
