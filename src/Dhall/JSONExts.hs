{-# LANGUAGE OverloadedStrings #-}

module Dhall.JSONExts (typeOf, normalize) where

import qualified Dhall.Context

import Dhall.Core      (Chunks, Const (..), Expr (..), ReifiedNormalizer (..),
                        Var (..), normalizeWith)
import Dhall.TypeCheck (TypeError, X, typeWith)


startingContext :: Dhall.Context.Context (Expr s a)
startingContext =
  transform Dhall.Context.empty
  where
    transformers = [ ("toJSON", Pi "t" (Const Type) (Pi "x" (Var (V "t" 0)) (Var (V "JSON" 0))))
                   , ("JSON", Const Type)]
    transform = (flip $ foldr (uncurry Dhall.Context.insert)) transformers

typeOf :: Expr s X -> Either (TypeError s X) (Expr s X)
typeOf = typeWith startingContext

normalize :: Eq a => Expr s a -> Expr t a
normalize = Dhall.Core.normalizeWith normalizer
  where normalizer (App (App (Var "toJSON") _) x) = pure (Just x)
        normalizer _ =
            pure Nothing

