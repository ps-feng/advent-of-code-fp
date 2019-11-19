module Day4
  ( day4Part1
  , Event
  , parseEventDateTime
  , parseEventType
  , EventType
  ) where

import Data.DateTime
import Data.Enum
import Data.Hashable
import Data.List.Types
import Data.String
import Data.String.Utils
import Data.Time
import Data.Time.Component
import Debug.Trace
import Prelude

import Control.Semigroupoid ((>>>))
import Data.Array (head, sort)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Formatter.DateTime (Formatter, parseFormatString, unformat, format)
import Data.Function ((#), flip)
import Data.HashMap as M
import Data.HashMap as M.HashMap
import Data.HashSet as S
import Data.Int (fromString)
import Data.List (List, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Util (readFileLines)

day4Part1 :: String -> Effect (Either String (Array Event))
day4Part1 filePath = do
  input <- readFileLines filePath
  let
    eitherRegex = regex "\\[(\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2})\\]\\s(Guard\\s#(\\d+)\\sbegins\\sshift|falls\\sasleep|wakes\\sup)" noFlags
  pure case eitherRegex of
    Left error -> Left error
    Right r ->
      let
        events = sort <$> (sequence $ map (match r >>> matchToEvent) input)
      in
        case events of
          Just theEvents -> Right theEvents
          Nothing -> Left "Empty claims"

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
  = S.HashSet Minute

type GuardSleepingSchedule
  = M.HashMap Date SleepingMinutes

type GuardIDToSleepingSchedule
  = M.HashMap GuardID GuardSleepingSchedule

--- [1518-11-01 00:00] Guard #10 begins shift  -- emptyMap, 10, 1518-11-01 00:00
--- [1518-11-01 00:05] falls asleep            -- emptyMap, 10, 1518-11-01 00:05
--- [1518-11-01 00:25] wakes up                -- 10->{Day1->Set 5-24}, 10, 1518-11-01 00:25
buildGuardIDToSleepingSchedule :: Array Event -> GuardIDToSleepingSchedule
buildGuardIDToSleepingSchedule events =
  let
    startingValue = { result: M.empty, guardID: -1, lastEventDateTime: distantPast }

    result = foldl calculateIntermediateResult startingValue events
  in
    result.result
  where
  distantPast = DateTime bottom bottom

  calculateIntermediateResult = \acc (Event event) -> case event.eventType of
    BeginsShift guardID -> { result: acc.result, guardID: guardID, lastEventDateTime: event.dateTime }
    FallsAsleep -> { result: acc.result, guardID: acc.guardID, lastEventDateTime: event.dateTime }
    WakesUp ->
      let
        sleepingStartingMinute = minute $ time acc.lastEventDateTime

        sleepingEndingMinute = minute $ time event.dateTime

        lastEventDate = date acc.lastEventDateTime

        sleepingMinutes :: SleepingMinutes
        sleepingMinutes =
          range (fromEnum sleepingStartingMinute) (fromEnum sleepingEndingMinute - 1)
            # map toEnum
            # sequence
            # fromMaybe Nil -- safe but incorrect... oh well
            # S.fromFoldable
        
        eventDate = date event.dateTime

        newResult :: GuardIDToSleepingSchedule
        newResult = M.alter (\maybeSchedule -> 
          case maybeSchedule of
            Nothing -> Just $ M.singleton lastEventDate sleepingMinutes
            Just schedule -> Just $ M.alter (\maybeSleepingMinutes -> Just sleepingMinutes
              -- case maybeSleepingMinutes of
              --   Nothing -> Just sleepingMinutes
              --   Just prevSleepingMinutes -> Just $ M.union prevSleepingMinutes sleepingMinutes
            ) lastEventDate schedule :: Maybe GuardSleepingSchedule
        ) acc.guardID acc.result
      in
        { result: newResult
        , guardID: acc.guardID
        , lastEventDateTime: event.dateTime
        }
