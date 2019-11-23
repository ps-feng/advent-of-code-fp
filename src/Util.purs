module Util
  ( parseAndApply
  , parseArrayOfNumbers
  , readTextFile
  , readFileLines
  , splitByNewline
  ) where

import Prelude

import Effect (Effect)
import Data.Either (Either, note)
import Data.Number (fromString)
import Data.String as String
import Data.Traversable (traverse)
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

-- Input parsing
type InputParser a = Array String -> Either String a

-- Converts the input into an array of `Number`s
parseArrayOfNumbers :: InputParser (Array Number)
parseArrayOfNumbers = note "An error occured parsing the input." <<< traverse fromString
    
parseAndApply :: forall a b. String -> InputParser a -> (a -> b) -> Effect (Either String b)
parseAndApply path parse f = do
  input <- readFileLines path
  let parsed = parse input
  pure $ f <$> parsed
