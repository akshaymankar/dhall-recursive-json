{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception)

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.JSONExts
import qualified Dhall.Parser

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
