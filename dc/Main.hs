{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dhall
import Fly.Types
import Fly.Interpret
import Data.Yaml
import Dhall.JSON

import qualified Data.Text.IO
import qualified Data.ByteString.Char8 as BC8

main :: IO ()
main = do
  stdin <- Data.Text.IO.getContents
  jobs <- input (list job) stdin
  BC8.putStrLn $ encode $ omitNull $ toJSON jobs
