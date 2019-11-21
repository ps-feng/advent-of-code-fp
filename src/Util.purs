module Util
  ( readTextFile
  , readFileLines
  , splitByNewline
  ) where

import Data.String as String
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FsSync
import Prelude (bind, pure, ($), (>=>))

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
