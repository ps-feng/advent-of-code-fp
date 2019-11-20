module Day4
  ( day4Part1
  , day4Part2
  , buildGuardIDToSleepingSchedule
  , sortedIntArrayMode
  , Event
  ) where

import Prelude (class Eq, class Ord, class Show, bind, bottom, compare, negate, otherwise, pure, show, (#), ($), (*), (-), (<$>), (<>), (>), (>=), (>>>))
import Data.Array (concat, group, insert, length, range, sort)
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime (DateTime(..), time)
import Data.Either (Either, hush, note)
import Data.Enum (fromEnum)
import Data.Foldable (foldl, maximumBy)
import Data.Formatter.DateTime (Formatter, parseFormatString, unformat)
import Data.Function (on)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (startsWith)
import Data.Time (minute)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Util (readFileLines)

day4Part1 :: String -> Effect (Either String Int)
day4Part1 filePath = solve filePath day4Part1Strategy

day4Part2 :: String -> Effect (Either String Int)
day4Part2 filePath = solve filePath day4Part2Strategy

type SolvingStrategy
  = (Array Event -> Maybe Int)

solve :: String -> SolvingStrategy -> Effect (Either String Int)
solve filePath strategy = do
  input <- readFileLines filePath
  pure do
    regex <- inputRegex
    let
      sortedEvents = sort <$> (traverse (match regex >>> matchToEvent) input)
    events <- note "Failed to parse claims" sortedEvents
    note "Failed to find result" $ strategy events

day4Part1Strategy :: Array Event -> Maybe Int
day4Part1Strategy events = do
  let
    sleepingSchedule = buildGuardIDToSleepingSchedule events
  let
    sleptMostGuardID = findGuardWithMostSleepingMinutes sleepingSchedule
  mostSleptMinute <- minuteGuardSpentMostSleeping sleptMostGuardID sleepingSchedule
  pure $ sleptMostGuardID * mostSleptMinute.minute

day4Part2Strategy :: Array Event -> Maybe Int
day4Part2Strategy events = do
  let
    sleepingSchedule = buildGuardIDToSleepingSchedule events
  let
    allMostSleptMins = findAllGuardToMostSleptMinute sleepingSchedule
  let
    Tuple guardID { minute } = findMostSleptGuardIDAndMinute allMostSleptMins
  pure $ guardID * minute

inputRegex :: Either String Regex
inputRegex = regex "\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] (Guard #(\\d+) begins shift|falls asleep|wakes up)" noFlags

data EventType
  = BeginsShift Int
  | FallsAsleep
  | WakesUp

derive instance eqEventType :: Eq EventType

instance showEventType :: Show EventType where
  show (BeginsShift guardID) = "BeginShift " <> show guardID
  show FallsAsleep = "FallsAsleep"
  show WakesUp = "WakesUp"

newtype Event
  = Event
  { dateTime :: DateTime
  , eventType :: EventType
  }

instance showEvent :: Show Event where
  show (Event event) = "<EventType dateTime: " <> show event.dateTime <> ", type: " <> show event.eventType <> ">"

derive instance eqEvent :: Eq Event

instance ordEvent :: Ord Event where
  compare (Event lhs) (Event rhs) = compare lhs.dateTime rhs.dateTime

matchToEvent :: Maybe (NonEmpty.NonEmptyArray (Maybe String)) -> Maybe Event
matchToEvent Nothing = Nothing

matchToEvent (Just eventMatch) =
  let
    eventGroups = NonEmpty.toArray eventMatch
  in
    case eventGroups of
      [ _, (Just dateTimeStr), (Just eventDescriptionStr), maybeGuardIDStr ] -> do
        dateTime <- hush $ parseEventDateTime dateTimeStr
        eventType <- parseEventType eventDescriptionStr maybeGuardIDStr
        pure $ Event { dateTime: dateTime, eventType: eventType }
      _ -> Nothing

parseEventType :: String -> Maybe String -> Maybe EventType
parseEventType s (Just guardIDStr)
  | startsWith "Guard" s = do
    guardID <- fromString guardIDStr
    pure $ BeginsShift guardID

parseEventType s Nothing
  | startsWith "falls" s = Just FallsAsleep
  | startsWith "wakes" s = Just WakesUp

parseEventType _ _ = Nothing

eventDateTimeFormatter :: Either String Formatter
eventDateTimeFormatter = parseFormatString "YYYY-MM-DD HH:mm"

parseEventDateTime :: String -> Either String DateTime
parseEventDateTime s = do
  formatter <- eventDateTimeFormatter
  unformat formatter s

type GuardID
  = Int

type SleepingMinutes
  = Array Int

type GuardSleepingSchedule
  = Array SleepingMinutes

type GuardIDToSleepingSchedule
  = M.Map GuardID GuardSleepingSchedule

type SleptMinute
  = { minute :: Int, times :: Int }

buildGuardIDToSleepingSchedule :: Array Event -> GuardIDToSleepingSchedule
buildGuardIDToSleepingSchedule events =
  foldl calculateIntermediateResult startingValue events
    # _.result
  where
  startingValue = { result: M.empty, guardID: -1, lastEventDateTime: distantPast }

  distantPast = DateTime bottom bottom

  calculateIntermediateResult acc (Event event) = case event.eventType of
    BeginsShift guardID -> acc { guardID = guardID, lastEventDateTime = event.dateTime }
    FallsAsleep -> acc { lastEventDateTime = event.dateTime }
    WakesUp ->
      let
        newResult = M.alter updateSleepingSchedule acc.guardID acc.result
      in
        acc { result = newResult, lastEventDateTime = event.dateTime }
      where
      updateSleepingSchedule maybeSchedule = case maybeSchedule of
        Nothing -> Just $ [ sleepingMinutes ]
        Just schedule -> Just $ sleepingMinutes `insert` schedule
    where
    sleepingStartingMinute = fromEnum $ minute $ time acc.lastEventDateTime

    sleepingEndingMinute = fromEnum $ minute $ time event.dateTime

    sleepingMinutes = range sleepingStartingMinute (sleepingEndingMinute - 1)

findGuardWithMostSleepingMinutes :: GuardIDToSleepingSchedule -> GuardID
findGuardWithMostSleepingMinutes guardIdToSleepingSchedule =
  foldl updateMostSleepingGuard { guardID: -1, numberOfSleepingMinutes: 0 } sleepingSchedule
    # _.guardID
  where
  updateMostSleepingGuard acc (Tuple guardID schedule) =
    if totalAmountOfSleepingMinutes > acc.numberOfSleepingMinutes then
      { guardID: guardID, numberOfSleepingMinutes: totalAmountOfSleepingMinutes }
    else
      acc
    where
    totalAmountOfSleepingMinutes = length $ concat schedule

  sleepingSchedule :: Array (Tuple GuardID GuardSleepingSchedule)
  sleepingSchedule = M.toUnfoldable guardIdToSleepingSchedule

findAllGuardToMostSleptMinute :: GuardIDToSleepingSchedule -> Array (Tuple GuardID SleptMinute)
findAllGuardToMostSleptMinute guardIdToSleepingSchedule = foldl updateGuards [] sleepingSchedule
  where
  updateGuards acc (Tuple guardID schedule) =
    let
      maybeMostSleptMinute = minuteGuardSpentMostSleeping guardID guardIdToSleepingSchedule
    in
      case maybeMostSleptMinute of
        Nothing -> acc
        Just mostSleptMinute -> (Tuple guardID mostSleptMinute) `insert` acc

  sleepingSchedule :: Array (Tuple GuardID GuardSleepingSchedule)
  sleepingSchedule = M.toUnfoldable guardIdToSleepingSchedule

minuteGuardSpentMostSleeping :: GuardID -> GuardIDToSleepingSchedule -> Maybe SleptMinute
minuteGuardSpentMostSleeping guardID guardIdToSleepingSchedule = do
  sleepingSchedule <- M.lookup guardID guardIdToSleepingSchedule
  let
    sortedMinutes = sort $ concat sleepingSchedule
  { number: number, times: times } <- sortedIntArrayMode sortedMinutes
  pure { minute: number, times: times }

findMostSleptGuardIDAndMinute :: Array (Tuple GuardID SleptMinute) -> (Tuple GuardID SleptMinute)
findMostSleptGuardIDAndMinute array = foldl maxSlept (Tuple (-1) { minute: -1, times: 0 }) array
  where
  maxSlept acc@(Tuple _ { times: accTimes }) elem@(Tuple _ { times: elemTimes })
    | accTimes >= elemTimes = acc
    | otherwise = elem

sortedIntArrayMode :: Array Int -> Maybe { number :: Int, times :: Int }
sortedIntArrayMode array = do
  longestGroup <-
    group array
      # maximumBy (compare `on` NonEmpty.length)
  pure { number: NonEmpty.head longestGroup, times: NonEmpty.length longestGroup }
