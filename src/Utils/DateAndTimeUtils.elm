module Utils.DateAndTimeUtils exposing 
    ( initTime
    , pickerTimeFromPosix 
    , pickerTimeNow
    , pickerTimeToMillis
    , addPickerTimeToPosix
    , dateToPosix
    , utcOffsetLabelsList
    , utcOffsetMinutesList
    , utcOffsetsPartsList
    , findIndexFromParts
    , offsetToParts
    , timeZoneToUtcOffsetMinutes
    , timeZoneToIndex
    , utcZeroOffsetIndex
    , joinDateTimeAndOffset
    , OffsetParts
    )

import Date exposing (Date)
import Iso8601
import Task exposing (Task)
import Time
import TimePicker as TP


type alias OffsetParts = 
    { multiplier : Int, hours : Int, minutes : Int }

initTime : TP.Time
initTime = pickerTimeFromPosix Time.utc <| Time.millisToPosix 0


pickerTimeFromPosix : Time.Zone -> Time.Posix -> TP.Time
pickerTimeFromPosix zone posix =
     let
        hour = Time.toHour zone posix
        minute = Time.toMinute zone posix
        second = Time.toSecond zone posix
    in
    { hours = hour, minutes = minute, seconds = second }


pickerTimeNow : Task Never TP.Time
pickerTimeNow =
    Task.map2 pickerTimeFromPosix Time.here Time.now


pickerTimeToMillis : Time.Zone -> TP.Time -> Int
pickerTimeToMillis zone time =
    let
        timeInSeconds = (time.hours * 60 * 60) + (time.minutes * 60) + (time.seconds)
        offsetInSeconds = (timeZoneToUtcOffsetMinutes zone) * 60
    in
    ( timeInSeconds - offsetInSeconds ) * 1000


pickerTimeWithOffsetToMillis : OffsetParts -> TP.Time -> Int
pickerTimeWithOffsetToMillis offset time =
    let
        timeInSeconds = (time.hours * 60 * 60) + (time.minutes * 60) + (time.seconds)
        offsetInSeconds = (partsToMinutes offset) * 60
    in
    ( timeInSeconds - offsetInSeconds ) * 1000


addPickerTimeToPosix : Time.Posix -> Time.Zone -> TP.Time -> Time.Posix
addPickerTimeToPosix posix zone pickerTime =
    (Time.posixToMillis posix) + (pickerTimeToMillis zone pickerTime) |>
        Time.millisToPosix


dateToPosix : Date -> Result String Time.Posix
dateToPosix date =
    case Date.toIsoString date |> Iso8601.toTime of
        Err _ ->
            Err "Failed to convert date to posix"

        Ok time ->
            Ok time


joinDateTimeAndOffset : Date -> TP.Time -> OffsetParts -> Time.Posix
joinDateTimeAndOffset date time offset =
    let
        dateMillis = Debug.log "DateMillis" <| Time.posixToMillis <|
                Maybe.withDefault (Time.millisToPosix 0) <|
                    Result.toMaybe <| dateToPosix date
        timeMillis = Debug.log "TimeMillis" <| pickerTimeWithOffsetToMillis offset time
    in
    Time.millisToPosix (dateMillis + timeMillis)

    

-- Time Zone Offset Calculations

timeZoneToUtcOffsetMinutes : Time.Zone -> Int
timeZoneToUtcOffsetMinutes zone =
    let
        oneDay = 24 * 60 * 60 * 1000
        
        hour = Time.toHour zone <| Time.millisToPosix oneDay
        minute = Time.toMinute zone <| Time.millisToPosix oneDay
        day = Time.toDay zone <| Time.millisToPosix oneDay

        hourUtc = Time.toHour Time.utc <| Time.millisToPosix oneDay
        minuteUtc = Time.toMinute Time.utc <| Time.millisToPosix oneDay
        dayUtc = Time.toDay Time.utc <| Time.millisToPosix oneDay

        hourOffset = (hour - hourUtc) * 60
        minuteOffset = minute - minuteUtc
        dayOffset = (day - dayUtc) * 24 * 60

        offset = dayOffset + hourOffset + minuteOffset
    in
    offset


utcOffsetMinutes : Int -> Int -> Int -> Int
utcOffsetMinutes multiplier hours minutes =
    -- multiplier is either 1 or -1 (for negative UTC offsets)
    multiplier * ((hours * 60) + minutes)


partsToMinutes : OffsetParts -> Int
partsToMinutes parts = utcOffsetMinutes parts.multiplier parts.hours parts.minutes


offsetToParts : Int -> OffsetParts
offsetToParts offset =
    let
        multiplier = if offset < 0 then -1 else 1

        hours = (abs offset) // 60
        minutes = modBy 60 <| abs offset
    in
    { multiplier = multiplier
    , hours = hours
    , minutes = minutes
    }


findIndexFromParts : OffsetParts -> Maybe Int
findIndexFromParts toSearch = 
    List.indexedMap Tuple.pair utcOffsetsPartsList |>
        List.filter (\(_, part) -> part == toSearch) |>
        List.map Tuple.first |>
        List.head


timeZoneToIndex : Time.Zone -> Maybe Int
timeZoneToIndex zone =
    timeZoneToUtcOffsetMinutes zone |>
        offsetToParts |>
            findIndexFromParts


partsToLabel : OffsetParts -> String
partsToLabel parts =
    case (parts.hours, parts.minutes) of
        (0, 0) -> "Z"
        (_, _) -> 
            let
                -- See: https://github.com/elm/compiler/issues/1773
                sign = 
                    case (parts.multiplier, negate parts.multiplier) of
                        (1, _) -> 
                            "+"

                        (_, 1) -> 
                            "-"

                        (_, _) -> 
                            " "
                
                hour = String.padLeft 2 '0' <| String.fromInt parts.hours
                minute = String.padLeft 2 '0' <| String.fromInt parts.minutes
            in
            (sign ++ hour ++ ":" ++ minute)


utcOffsetLabelsList : List String
utcOffsetLabelsList = List.map partsToLabel utcOffsetsPartsList


utcOffsetMinutesList : List Int
utcOffsetMinutesList = List.map partsToMinutes utcOffsetsPartsList


utcZeroOffsetIndex : Int
utcZeroOffsetIndex = 14


utcOffsetsPartsList : List OffsetParts
utcOffsetsPartsList =
    [ { multiplier = -1
      , hours = 12
      , minutes = 0 } -- 0
    , { multiplier = -1
      , hours = 11
      , minutes = 0 } -- 1
    , { multiplier = -1
      , hours = 10
      , minutes = 0 } -- 2
    , { multiplier = -1
      , hours = 9
      , minutes = 30 } -- 3
    , { multiplier = -1
      , hours = 9
      , minutes = 0 } -- 4
    , { multiplier = -1
      , hours = 8
      , minutes = 0 } -- 5
    , { multiplier = -1
      , hours = 7
      , minutes = 0 } -- 6
    , { multiplier = -1
      , hours = 6
      , minutes = 0 } -- 7
    , { multiplier = -1
      , hours = 5
      , minutes = 0 } -- 8
    , { multiplier = -1
      , hours = 4
      , minutes = 0 } -- 9
    , { multiplier = -1
      , hours = 3
      , minutes = 30 } -- 10
    , { multiplier = -1
      , hours = 3
      , minutes = 0 } -- 11
    , { multiplier = -1
      , hours = 2
      , minutes = 0 } -- 12 
    , { multiplier = -1
      , hours = 1
      , minutes = 0 } -- 13
    , { multiplier = 1
      , hours = 0
      , minutes = 0 } -- 14
    , { multiplier = 1
      , hours = 1
      , minutes = 0 } -- 15
    , { multiplier = 1
      , hours = 2
      , minutes = 0 } -- 16
    , { multiplier = 1
      , hours = 3
      , minutes = 0 } -- 17
    , { multiplier = 1
      , hours = 3
      , minutes = 30 } -- 18
    , { multiplier = 1
      , hours = 4
      , minutes = 0 } -- 19
    , { multiplier = 1
      , hours = 4
      , minutes = 30 } -- 20
    , { multiplier = 1
      , hours = 5
      , minutes = 0 } -- 21
    , { multiplier = 1
      , hours = 5
      , minutes = 30 } -- 22
    , { multiplier = 1
      , hours = 5
      , minutes = 45 } -- 23
    , { multiplier = 1
      , hours = 6
      , minutes = 0 } -- 24
    , { multiplier = 1
      , hours = 6
      , minutes = 30 } -- 25
    , { multiplier = 1
      , hours = 7
      , minutes = 0 } -- 26
    , { multiplier = 1
      , hours = 8
      , minutes = 0 } -- 27
    , { multiplier = 1
      , hours = 8
      , minutes = 45 } -- 28
    , { multiplier = 1
      , hours = 9
      , minutes = 0 } -- 29
    , { multiplier = 1
      , hours = 9
      , minutes = 30 } -- 30
    , { multiplier = 1
      , hours = 10
      , minutes = 0 } -- 31
    , { multiplier = 1
      , hours = 10
      , minutes = 30 } -- 32
    , { multiplier = 1
      , hours = 11
      , minutes = 0 } -- 33
    , { multiplier = 1
      , hours = 12
      , minutes = 0 } -- 34
    , { multiplier = 1
      , hours = 12
      , minutes = 45 } -- 35
    , { multiplier = 1
      , hours = 13
      , minutes = 0 } -- 36
    , { multiplier = 1
      , hours = 14
      , minutes = 0 } -- 37
    ]