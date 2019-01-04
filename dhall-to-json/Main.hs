{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception         (Exception, SomeException)
import Data.Text                 (pack)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Core                (Chunks, Const (..), Expr (..),
                                  ReifiedNormalizer (..), Var (..),
                                  normalizeWith)
import Dhall.Pretty              (Ann, CharacterSet (..), annToAnsiStyle,
                                  layoutOpts)
import System.IO                 (Handle)

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Map                                 as Map
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Lens.Family                               as Lens
import qualified System.Console.ANSI
import qualified System.IO

import qualified Dhall.JSONExts

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right a) = return a

main :: IO ()
main = do
    text <- Data.Text.IO.getContents
    expression <- throws (Dhall.Parser.exprFromText "(stdin)" text)

    resolvedExpression <- State.evalStateT (Dhall.Import.loadWith expression) (Dhall.Import.emptyStatus ".")

    inferredType <- throws (Dhall.JSONExts.typeOf resolvedExpression)

    let normalizedExpression = Dhall.JSONExts.normalize resolvedExpression

    case Dhall.JSON.dhallToJSON normalizedExpression of
      Left err -> Control.Exception.throwIO err
      Right json -> Data.ByteString.Char8.putStrLn $ Data.ByteString.Lazy.toStrict $ Data.Aeson.Encode.Pretty.encodePretty json
