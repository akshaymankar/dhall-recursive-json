{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import qualified Dhall.JSONExts
import qualified Data.Text.IO



main :: IO ()
main = do
  -- let status = set Dhall.Import.standardVersion standardVersion (Dhall.Import.emptyStatus ".")
  stdin <- Data.Text.IO.getContents
  expression <- throws (Dhall.Parser.exprFromText (Data.Text.unpack "(stdin)") stdin)

  resolvedExpression <- State.evalStateT (Dhall.Import.loadWith expression) (Lens.set Dhall.Import.startingContext Dhall.JSONExts.startingContext $ Dhall.Import.emptyStatus ".")

  inferredType <- throws (Dhall.JSONExts.typeOf resolvedExpression)

  let normalizedExpression = Dhall.JSONExts.normalize resolvedExpression

  print (normalizedExpression :: Expr Dhall.Parser.Src X)

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right a) = return a
