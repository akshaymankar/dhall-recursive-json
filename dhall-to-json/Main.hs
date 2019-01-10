{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception   (SomeException)
import Control.Monad       (when)
import Data.Monoid         ((<>))
import Dhall.JSON          (Conversion)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.JSON
import qualified Dhall.JSONExts
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.Exit
import qualified System.IO


data Options = Options
    { explain    :: Bool
    , pretty     :: Bool
    , omitNull   :: Bool
    , version    :: Bool
    , conversion :: Conversion
    }

parseOptions :: Parser Options
parseOptions = Options.Applicative.helper <*> do
    explain    <- parseExplain
    pretty     <- parsePretty
    omitNull   <- parseOmitNull
    version    <- parseVersion
    conversion <- Dhall.JSON.parseConversion
    return (Options {..})
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in detail"
            )

    parsePretty =
        prettyFlag <|> compactFlag <|> defaultBehavior
      where
        prettyFlag =
            Options.Applicative.flag'
                True
                (   Options.Applicative.long "pretty"
                <>  Options.Applicative.help "Pretty print generated JSON"
                )

        compactFlag =
            Options.Applicative.flag'
                False
                (   Options.Applicative.long "compact"
                <>  Options.Applicative.help "Render JSON on one line"
                )

        defaultBehavior =
            pure False

    parseOmitNull =
        Options.Applicative.switch
            (   Options.Applicative.long "omitNull"
            <>  Options.Applicative.help "Omit record fields that are null"
            )

    parseVersion =
        Options.Applicative.switch
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        parseOptions
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc "Compile Dhall to JSON"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options {..} <- Options.Applicative.execParser parserInfo

    when version $ do
      putStrLn "Unsupported"
      System.Exit.exitSuccess

    handle $ do
        let config = Data.Aeson.Encode.Pretty.Config
                       { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Spaces 2
                       , Data.Aeson.Encode.Pretty.confCompare = compare
                       , Data.Aeson.Encode.Pretty.confNumFormat = Data.Aeson.Encode.Pretty.Generic
                       , Data.Aeson.Encode.Pretty.confTrailingNewline = False }
        let encode =
                if pretty
                then Data.Aeson.Encode.Pretty.encodePretty' config
                else Data.Aeson.encode

        let explaining = if explain then Dhall.detailed else id

        let omittingNull = if omitNull then Dhall.JSON.omitNull else id

        stdin <- Data.Text.IO.getContents

        json <- omittingNull <$> explaining (Dhall.JSONExts.codeToValue conversion "(stdin)" stdin)

        Data.ByteString.Char8.putStrLn $ Data.ByteString.Lazy.toStrict $ encode json

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
