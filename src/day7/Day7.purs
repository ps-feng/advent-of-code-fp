module Day7 
  ( day7Part1
  ) where

import Prelude

import Data.Foldable(foldl)
import Data.List ((:))
import Data.List (List(..)) as L
import Data.Map (Map, alter, empty, keys, lookup) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.PQueue (PQueue, empty, fromFoldable, head, insert, tail) as PQ
import Data.Set (difference, fromFoldable, map) as S
import Data.String.CodeUnits (charAt, fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Util (readFileLines)

type Step = Char

type Instruction = 
  { from :: Step
  , to :: Step 
  }

type AdjacencyMap =
  M.Map Step (L.List Step)

type InDegreeMap =
  M.Map Step Int
  
type ZeroInDegreeQueue =
  PQ.PQueue Step Step

type State = 
  { adjacencyMap :: AdjacencyMap
  , inDegreeMap :: InDegreeMap
  , zeroInDegreeQueue :: ZeroInDegreeQueue
  }

day7Part1 :: String -> Effect String
day7Part1 filePath = do
  input <- readFileLines filePath
  pure $ case sequence $ lineToInstruction <$> input of
    Just instructions -> fromCharArray $ solve instructions
    Nothing -> "Ship! It's broken :("
  where
    lineToInstruction :: String -> Maybe Instruction
    lineToInstruction line = do
      from <- charAt 5 line
      to <- charAt 36 line
      pure $ { from, to }

solve :: Array Instruction -> Array Step
solve steps = 
  let 
    state@{ adjacencyMap, inDegreeMap, zeroInDegreeQueue } = buildInitialState steps
  in 
    calculateSteps state

buildInitialState :: Array Instruction -> State
buildInitialState instructions =
  let 
    { adjacencyMap, inDegreeMap } = foldl updateStateWithInstruction { adjacencyMap: M.empty, inDegreeMap: M.empty } instructions
    zeroInDegreeQueue = PQ.fromFoldable $ S.map (\v -> Tuple v v) $ S.difference (S.fromFoldable $ M.keys adjacencyMap) (S.fromFoldable $ M.keys inDegreeMap)
  in 
    { adjacencyMap: adjacencyMap
    , inDegreeMap: inDegreeMap
    , zeroInDegreeQueue: zeroInDegreeQueue
    }
  where
    updateStateWithInstruction :: { adjacencyMap :: AdjacencyMap , inDegreeMap :: InDegreeMap } 
                               -> Instruction 
                               -> { adjacencyMap :: AdjacencyMap , inDegreeMap :: InDegreeMap } 
    updateStateWithInstruction { adjacencyMap, inDegreeMap } { from, to } = 
      { adjacencyMap: M.alter (\v -> Just $ to : (fromMaybe L.Nil v)) from adjacencyMap
      , inDegreeMap: M.alter (\v -> Just $ 1 + (fromMaybe 0 v)) to inDegreeMap
      }

calculateSteps :: State -> Array Step
calculateSteps state =
  calculateSteps' [] state
  where
    calculateSteps' :: Array Step -> State -> Array Step
    calculateSteps' currentSteps { adjacencyMap, inDegreeMap, zeroInDegreeQueue } = 
      case PQ.head zeroInDegreeQueue of
        Just (Tuple _ nextStep) -> 
            let
              adjacents = getAdjacents nextStep adjacencyMap
              nextInDegreeMap = calculateNextInDegreeMap inDegreeMap adjacents
              nextZeroInDegreeQueue = calculateNextZeroInDegreeQueue (fromMaybe PQ.empty $ PQ.tail zeroInDegreeQueue) nextInDegreeMap adjacents
            in calculateSteps' (currentSteps <> [nextStep]) { adjacencyMap, inDegreeMap: nextInDegreeMap, zeroInDegreeQueue: nextZeroInDegreeQueue} 
        Nothing -> currentSteps

    getAdjacents :: Step -> AdjacencyMap -> L.List Step
    getAdjacents nextStep adjacencyMap = fromMaybe L.Nil $ M.lookup nextStep adjacencyMap

    calculateNextInDegreeMap :: InDegreeMap -> L.List Step -> InDegreeMap
    calculateNextInDegreeMap inDegreeMap adjacents = foldl (\acc adj -> M.alter (\v -> (_-1) <$> v) adj acc) inDegreeMap adjacents

    calculateNextZeroInDegreeQueue :: ZeroInDegreeQueue -> InDegreeMap -> L.List Step -> ZeroInDegreeQueue
    calculateNextZeroInDegreeQueue zeroInDegreeQueue inDegreeMap adjacents = 
        foldl (
          \acc adj -> 
            case M.lookup adj inDegreeMap of
              Just inDegree | inDegree == 0 -> PQ.insert adj adj acc
              _ -> acc
        ) zeroInDegreeQueue adjacents


-- Test input for repl:
-- [ { from:'C', to: 'A' }, { from:'C', to: 'F' }, { from:'A', to: 'B' }, { from:'A', to: 'D' }, { from:'B', to: 'E' }, { from:'D', to: 'E' }, { from:'F', to: 'E' } ]