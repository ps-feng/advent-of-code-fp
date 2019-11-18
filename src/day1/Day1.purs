module Day1 where

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number (fromString, nan)
import Data.Traversable (sequence)
import Data.String as String
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FsSync
import Prelude

day1 :: Effect Number
day1 = do
  input <- readFileLines "test/day1/input.txt"
  let parsed = fromString <$> input -- Array (Optional Number)
  let floats = sequence parsed -- Optional (Array Number)
  let result = foldl (+) 0.0 <$> floats
  case result of
    Just x -> pure x
    Nothing -> pure nan
  
readFileLines :: String -> Effect (Array String)
readFileLines filePath = do
  text <- FsSync.readTextFile UTF8 filePath
  let
    lines = splitByNewline text
  pure $ splitByNewline text

splitByNewline ∷ String → Array String
splitByNewline =
  String.split (String.Pattern "\n")
    >=> String.split (String.Pattern "\r")
