{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.JSONExts (typeOf, startingContext, normalize, codeToValue) where


import Control.Exception (Exception)
import Data.Aeson        (Value)
import Data.Text         (Text)
import Dhall.Core        (Chunks, Const (..), Expr (..), ReifiedNormalizer (..),
                          Var (..), normalizeWith)
import Dhall.JSON        (Conversion)
import Dhall.TypeCheck   (TypeError, X, typeWith)

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Parser
import qualified Lens.Family                      as Lens

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


codeToValue :: Conversion -> Text -> Text -> IO Value
codeToValue conversion name code = do
  expression <- throws (Dhall.Parser.exprFromText (Data.Text.unpack name) code)

  resolvedExpression <- State.evalStateT (Dhall.Import.loadWith expression) (Lens.set Dhall.Import.startingContext startingContext $ Dhall.Import.emptyStatus ".")

  inferredType <- throws (Dhall.JSONExts.typeOf resolvedExpression)

  let normalizedExpression = normalize resolvedExpression

  let convertedExpression = Dhall.JSON.convertToHomogeneousMaps conversion normalizedExpression

  case Dhall.JSON.dhallToJSON convertedExpression of
    Left err   -> Control.Exception.throwIO err
    Right json -> return json

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right a) = return a
