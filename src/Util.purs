module Util
  ( readFileLines
  , splitByNewline
  ) where

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FsSync
import Prelude (bind, pure, ($), (>=>))
import Data.String as String

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
