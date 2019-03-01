module Utils.DateAndTimeUtils exposing 
    ( initTime
    , pickerTimeFromPosix 
    , pickerTimeNow
    , pickerTimeToMillis
    , addPickerTimeToPosix
    , dateToPosix
    )

import Date exposing (Date)
import Iso8601
import Task exposing (Task)
import Time
import TimePicker as TP


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
    ( timeInSeconds + offsetInSeconds ) * 1000


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


offsetToParts : Int -> { multiplier : Int, hours : Int, minutes : Int }
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