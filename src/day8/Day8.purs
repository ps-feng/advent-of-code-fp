module Day8 
  ( day8Part1
  , nodeParser
  , Node
  , sumMetadata
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable) as A
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Int (fromString)
import Data.List (List(..), fromFoldable, length) as L
import Data.List.Lazy (replicateM)
import Data.List.Lazy (toUnfoldable) as LL
import Data.List.Lazy.NonEmpty as L
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.CodePoints (anyDigit, char, eof, skipSpaces, string, anyChar, regex)
import Text.Parsing.StringParser.Combinators (many1, endBy1, sepBy1, optionMaybe, many, manyTill, many1Till, chainl, fix, between)
import Text.Parsing.StringParser.Expr (Assoc(..), Operator(..), buildExprParser)
import Util (readTextFile)

type Metadata = Int
data Node = Node (L.List Node) (L.List Metadata)
type AST = L.List Node

instance showNode :: Show Node where
  show (Node children metadata) = "Node " <> show children <> " " <> show metadata

day8Part1 :: String -> Effect Int
day8Part1 filePath = do
  input <- readTextFile filePath
  case runParser nodeParser input of
    Left _ -> throwException $ error "Something wrong with the input"
    Right ast -> pure $ sumMetadata ast

numberParser :: Parser Int
numberParser = do
  numberCharList <- many1 anyDigit
  let numberStr = fromCharArray $ A.fromFoldable numberCharList
  pure $ fromMaybe 0 $ fromString numberStr

nodeParser :: Parser Node
nodeParser = do
  numChildren <- skipSpaces *> numberParser
  numMetadata <- skipSpaces *> numberParser
  children <- skipSpaces *> replicateA numChildren nodeParser
  metadata <- skipSpaces *> replicateA numMetadata (skipSpaces *> numberParser)
  pure $ Node children metadata

sumMetadata :: Node -> Int
sumMetadata root =
  sumMetadata' 0 root
  where
    sumMetadata' acc (Node L.Nil metadata) = acc + sum metadata
    sumMetadata' acc (Node children metadata) =
      foldl sumMetadata' acc children + sum metadata
