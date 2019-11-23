module Util
  ( readTextFile
  , readFileLines
  , splitByNewline
  , time
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log, time, timeEnd) as Console
import Data.String as String
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FsSync

readTextFile :: String -> Effect String
readTextFile filePath = FsSync.readTextFile UTF8 filePath

readFileLines :: String -> Effect (Array String)
readFileLines filePath = do
  text <- readTextFile filePath
  let
    lines = splitByNewline text
  pure $ splitByNewline text

splitByNewline ∷ String → Array String
splitByNewline =
  String.split (String.Pattern "\n")
    >=> String.split (String.Pattern "\r")

time :: forall a. Show a => (Unit -> a) -> Effect Unit
time op = do
  Console.time "timer"
  Console.log $ show $ op unit
  Console.timeEnd "timer" 
    