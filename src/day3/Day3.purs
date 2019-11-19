module Day3
  ( day3Part1
  , numberOfOverlappingClaims
  , Claim(..)
  , updateFabric
  ) where

import Control.Semigroupoid ((>>>))
import Data.Either (Either(..))
import Data.Function (flip)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Array.NonEmpty as NonEmpty
import Data.Foldable (foldl)
import Data.Hashable
import Data.HashMap as H
import Data.Int (fromString)
import Data.List (List, range)
import Data.Maybe (Maybe(..))
import Data.Ord ((>))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Prelude (bind, map, pure, ($), (+), (-))
import Util (readFileLines)

day3Part1 :: String -> Effect (Either String Int)
day3Part1 filePath= do
  input <- readFileLines filePath
  let
    eitherRegex = regex "#(\\d+)\\s@\\s(\\d+),(\\d+):\\s(\\d+)x(\\d+)" noFlags
  pure case eitherRegex of
    Left error -> Left error
    Right r ->
      let
        claims = sequence $ map (match r >>> matchToClaim) input
      in
        case claims of
          Just theClaims -> Right $ numberOfOverlappingClaims theClaims
          Nothing -> Left "Empty claims"

newtype Claim
  = Claim
  { id :: Int
  , x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  }

type Coord
  = Tuple Int Int

type OverlapCount
  = Int

type Fabric
  = H.HashMap Coord OverlapCount

numberOfOverlappingClaims :: Array Claim -> Int
numberOfOverlappingClaims claims =
  let fabric = foldl (flip updateFabric) H.empty claims
      squaresWithOverlappingClaims = H.filter (\overlapCount -> overlapCount > 1) fabric
  in H.size squaresWithOverlappingClaims
    
updateFabric :: Claim -> Fabric -> Fabric
updateFabric claim fabric = foldl alterFabric fabric squares
  where
  alterFabric = \accumFabric square -> alterFabric' square accumFabric

  alterFabric' :: forall k. Hashable k => k -> H.HashMap k Int -> H.HashMap k Int
  alterFabric' =
    H.alter
      ( \value -> case value of
          Nothing -> Just 1
          Just prevValue -> Just $ prevValue + 1
      )

  squares = claimedSquares claim

claimedSquares :: Claim -> List Coord
claimedSquares (Claim c) = do
  a <- range c.x (c.x + c.width - 1)
  b <- range c.y (c.y + c.height - 1)
  pure $ Tuple a b

matchToClaim :: Maybe (NonEmpty.NonEmptyArray (Maybe String)) -> Maybe Claim
matchToClaim Nothing = Nothing

matchToClaim (Just matches) =
  let
    matchesArray = NonEmpty.toArray matches
  in
    case matchesArray of
      [ (Just _), (Just id), (Just x), (Just y), (Just width), (Just height) ] -> do
        id' <- fromString id
        x' <- fromString x
        y' <- fromString y
        width' <- fromString width
        height' <- fromString height
        pure
          $ Claim
              { id: id'
              , x: x'
              , y: y'
              , width: width'
              , height: height'
              }
      _ -> Nothing
