module Day7 
  ( day7Part1
  , day7Part2
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.List ((:))
import Data.List (List(..), length, mapMaybe) as L
import Data.Map (Map, alter, empty, keys, lookup) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.PQueue (PQueue, empty, fromFoldable, head, insert, tail) as PQ
import Data.Set (difference, fromFoldable, map) as S
import Data.String.CodeUnits (charAt, fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Util (readFileLines)

import Debug.Trace (trace)

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

type SchedulerQueue =
  PQ.PQueue Int Step

type State = 
  { adjacencyMap :: AdjacencyMap
  , inDegreeMap :: InDegreeMap
  , zeroInDegreeQueue :: ZeroInDegreeQueue
  }

type State2 = 
  { adjacencyMap :: AdjacencyMap
  , inDegreeMap :: InDegreeMap
  , zeroInDegreeQueue :: ZeroInDegreeQueue
  , schedulerQueue :: SchedulerQueue
  }

parseInstructions :: String -> Effect (Maybe (Array Instruction))
parseInstructions filePath = do
  input <- readFileLines filePath
  pure $ sequence $ lineToInstruction <$> input
  where
    lineToInstruction :: String -> Maybe Instruction
    lineToInstruction line = do
      from <- charAt 5 line
      to <- charAt 36 line
      pure $ { from, to }

day7Part1 :: String -> Effect String
day7Part1 filePath = do
  maybeInstructions <- parseInstructions filePath
  pure $ case maybeInstructions of
    Just instructions -> fromCharArray $ solve1 instructions
    Nothing -> "Ship! It's broken :("

day7Part2 :: String -> Int -> Effect Int
day7Part2 filePath numWorkers = do
  maybeInstructions <- parseInstructions filePath
  case maybeInstructions of
    Just instructions -> pure $ solve2 instructions numWorkers
    Nothing -> throwException $ error "Ship! It's broken :("

solve1 :: Array Instruction -> Array Step
solve1 steps = 
  let 
    state@{ adjacencyMap, inDegreeMap, zeroInDegreeQueue } = buildInitialState steps
  in 
    calculatePart1 [] state

solve2 :: Array Instruction -> Int -> Int
solve2 steps numWorkers = 
  let 
    { adjacencyMap, inDegreeMap, zeroInDegreeQueue } = buildInitialState steps
  in
    calculatePart2 
      numWorkers 0
      { adjacencyMap
      , inDegreeMap
      , zeroInDegreeQueue
      , schedulerQueue: PQ.empty 
      }

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

calculatePart1 :: Array Step -> State -> Array Step
calculatePart1 currentSteps { adjacencyMap, inDegreeMap, zeroInDegreeQueue } = 
  case PQ.head zeroInDegreeQueue of
    Just (Tuple _ nextStep) ->
        let
          adjacents = getAdjacents nextStep adjacencyMap
          nextInDegreeMap = calculateNextInDegreeMap inDegreeMap adjacents
          nextZeroInDegreeQueue = calculateNextZeroInDegreeQueue (fromMaybe PQ.empty $ PQ.tail zeroInDegreeQueue) nextInDegreeMap adjacents
        in calculatePart1 (currentSteps <> [nextStep]) { adjacencyMap, inDegreeMap: nextInDegreeMap, zeroInDegreeQueue: nextZeroInDegreeQueue} 
    Nothing -> currentSteps

calculatePart2 :: Int -> Int -> State2 -> Int
calculatePart2 numWorkers timeElapsed { adjacencyMap, inDegreeMap, zeroInDegreeQueue, schedulerQueue }
  | pqlength zeroInDegreeQueue == 0 && pqlength schedulerQueue == 0 = timeElapsed
  | otherwise =
  let
    { zeroInDegreeQueue: intermediateZeroInDegreeQueue
    , schedulerQueue: intermediateSchedulerQueue } = 
      moveToSchedulerQueue zeroInDegreeQueue schedulerQueue

    adjacents =
      case PQ.head zeroInDegreeQueue of
        Nothing -> L.Nil
        Just (Tuple _ nextStep) -> getAdjacents nextStep adjacencyMap
    
    nextInDegreeMap = calculateNextInDegreeMap inDegreeMap adjacents
    
    nextZeroInDegreeQueue =
      if pqlength schedulerQueue < numWorkers then
        calculateNextZeroInDegreeQueue 
          (fromMaybe PQ.empty $ PQ.tail intermediateZeroInDegreeQueue) 
          nextInDegreeMap
          adjacents
      else
        intermediateZeroInDegreeQueue
    
    bla3 = trace ("Time " <> show timeElapsed <> " zero-in: " <> show zeroInDegreeQueue) \_-> 
            trace ("Time " <> show timeElapsed <> " intermZQ: " <> show intermediateZeroInDegreeQueue) \_-> 
              trace ("Time " <> show timeElapsed <> " next zero-in: " <> show nextZeroInDegreeQueue) \_-> 1

    (Tuple time nextSchedulerQueue) = finishOneStep intermediateSchedulerQueue
  in calculatePart2
      numWorkers
      (timeElapsed + time)
      { adjacencyMap
      , inDegreeMap: nextInDegreeMap
      , zeroInDegreeQueue: nextZeroInDegreeQueue
      , schedulerQueue: nextSchedulerQueue }
  where
    moveToSchedulerQueue :: ZeroInDegreeQueue
                         -> SchedulerQueue
                         -> { zeroInDegreeQueue :: ZeroInDegreeQueue, schedulerQueue :: SchedulerQueue }
    moveToSchedulerQueue zqueue schedQueue =
      let
        numStepsToMove = min (numWorkers - pqlength schedQueue) (pqlength zqueue)
      in
        move numStepsToMove zqueue schedQueue

    finishOneStep :: SchedulerQueue -> Tuple Int SchedulerQueue
    finishOneStep schedQueue =
      let head = PQ.head (trace ("Processing 1 step, queue: " <> show schedQueue) \_-> schedQueue)
      in
        case head of
          Nothing -> Tuple 0 schedQueue
          Just (Tuple prio step) -> 
            let bla = pqmapMaybePrio substractPrio schedQueue
            in Tuple prio $ trace ("Finished, queue: " <> show bla) \_-> bla
                where
                  substractPrio oldPrio =
                    case oldPrio - prio of
                      0 -> Nothing
                      updatedPrio -> Just updatedPrio

getAdjacents :: Step -> AdjacencyMap -> L.List Step
getAdjacents nextStep adjacencyMap = fromMaybe L.Nil $ M.lookup nextStep adjacencyMap

calculateNextInDegreeMap :: InDegreeMap -> L.List Step -> InDegreeMap
calculateNextInDegreeMap inDegreeMap adjacents = foldl (\acc adj -> M.alter (\v -> (_-1) <$> v) adj acc) inDegreeMap adjacents

calculateNextZeroInDegreeQueue :: ZeroInDegreeQueue -> InDegreeMap -> L.List Step -> ZeroInDegreeQueue
calculateNextZeroInDegreeQueue zqueue inMap adjacents = 
  foldl (
    \acc adj -> 
      case M.lookup adj inMap of
        Just inDegree | inDegree == 0 -> PQ.insert adj adj acc
        _ -> acc
  ) zqueue adjacents

move :: Int -> ZeroInDegreeQueue -> SchedulerQueue -> { zeroInDegreeQueue :: ZeroInDegreeQueue, schedulerQueue :: SchedulerQueue }
move 0 zeroInDegreeQueue schedulerQueue = { zeroInDegreeQueue, schedulerQueue }
move num zeroInDegreeQueue schedulerQueue  =
  let
    head = PQ.head zeroInDegreeQueue
  in
    case head of
      Nothing -> { zeroInDegreeQueue, schedulerQueue }
      Just (Tuple p a) ->
        let
          schedPriority = (toCharCode a) - (toCharCode 'A') + 1
          newSchedulerQueue = PQ.insert schedPriority a schedulerQueue
        in case PQ.tail zeroInDegreeQueue of
            Nothing -> { zeroInDegreeQueue: PQ.empty, schedulerQueue: newSchedulerQueue }
            Just zeroInDegreeQueue' -> move (num - 1) zeroInDegreeQueue' newSchedulerQueue

pqlength :: forall p a. PQ.PQueue p a -> Int
pqlength = unwrap >>> L.length

pqmapMaybePrio :: forall p q a . Ord p => Ord q =>
                  (p -> Maybe q) -> PQ.PQueue p a -> PQ.PQueue q a
pqmapMaybePrio mapper queue =
  let 
    unwrappedQueue = unwrap queue
    updatedList = 
      L.mapMaybe
        (\(Tuple p a) ->
          case mapper(p) of
            Nothing -> Nothing
            Just newP -> Just (Tuple newP a))
        unwrappedQueue
  in
    PQ.fromFoldable updatedList

-- Test input for repl:
-- [ { from:'C', to: 'A' }, { from:'C', to: 'F' }, { from:'A', to: 'B' }, { from:'A', to: 'D' }, { from:'B', to: 'E' }, { from:'D', to: 'E' }, { from:'F', to: 'E' } ]