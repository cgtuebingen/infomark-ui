module Utils.DateFormatter exposing 
    ( dateAndTimeFormatter
    , dateToPosix
    , dateToShortFormatString
    , dayFormatter
    , fullDateFormatter
    , monthFormatter
    , shortDateFormatter
    , shortDayFormatter
    , timeFormatter
    , joinDateAndTime
    , timeZoneToUtcOffsetMinutes
    )

import Date exposing (Date)
import Html exposing (..)
import I18n
import Iso8601
import SharedState exposing (SharedState)
import Time exposing (Posix, Zone(..))
import TimePicker


dayFormatter : SharedState -> Time.Weekday -> String
dayFormatter sharedState day =
    let
        t =
            I18n.get sharedState.translations
    in
    case day of
        Time.Mon ->
            t "day-mon"

        Time.Tue ->
            t "day-tue"

        Time.Wed ->
            t "day-wed"

        Time.Thu ->
            t "day-thu"

        Time.Fri ->
            t "day-fri"

        Time.Sat ->
            t "day-sat"

        Time.Sun ->
            t "day-sun"


monthFormatter : SharedState -> Time.Month -> String
monthFormatter sharedState month =
    let
        t =
            I18n.get sharedState.translations
    in
    case month of
        Time.Jan ->
            t "month-jan"

        Time.Feb ->
            t "month-feb"

        Time.Mar ->
            t "month-mar"

        Time.Apr ->
            t "month-apr"

        Time.May ->
            t "month-may"

        Time.Jun ->
            t "month-jun"

        Time.Jul ->
            t "month-jul"

        Time.Aug ->
            t "month-aug"

        Time.Sep ->
            t "month-sep"

        Time.Oct ->
            t "month-oct"

        Time.Nov ->
            t "month-nov"

        Time.Dec ->
            t "month-dec"


shortDayFormatter : SharedState -> Time.Weekday -> String
shortDayFormatter sharedState day =
    let
        t =
            I18n.get sharedState.translations
    in
    case day of
        Time.Mon ->
            t "day-mon-short"

        Time.Tue ->
            t "day-tue-short"

        Time.Wed ->
            t "day-wed-short"

        Time.Thu ->
            t "day-thu-short"

        Time.Fri ->
            t "day-fri-short"

        Time.Sat ->
            t "day-sat-short"

        Time.Sun ->
            t "day-sun-short"


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


dateToPosix : Date -> Result String Posix
dateToPosix date =
    case Date.toIsoString date |> Iso8601.toTime of
        Err _ ->
            Err "Failed to convert date to posix"

        Ok time ->
            Ok time


dateToShortFormatString : SharedState -> Date -> String
dateToShortFormatString sharedState date =
    let
        curTime =
            Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime
    in
    Result.withDefault curTime (dateToPosix date)
        |> shortDateFormatter sharedState


timeFormatter : SharedState -> Posix -> String
timeFormatter sharedState time =
    let
        hour =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone
                        |> flip Time.toHour time
                    )

        minute =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone
                        |> flip Time.toMinute time
                    )

        second =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone
                        |> flip Time.toSecond time
                    )
    in
    hour ++ ":" ++ minute ++ ":" ++ second


shortDateFormatter : SharedState -> Posix -> String
shortDateFormatter sharedState time =
    let
        day =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone
                        |> flip Time.toDay time
                    )

        monthType =
            Maybe.withDefault Time.utc sharedState.timezone
                |> flip Time.toMonth time

        month =
            monthFormatter sharedState monthType

        year =
            String.fromInt
                (Maybe.withDefault Time.utc sharedState.timezone
                    |> flip Time.toYear time
                )
    in
    day ++ "/" ++ month ++ "/" ++ year


dateAndTimeFormatter : SharedState -> Posix -> Html msg
dateAndTimeFormatter sharedState time =
    let
        timeFormat =
            timeFormatter sharedState time

        dateFormat =
            shortDateFormatter sharedState time
    in
    span []
        [ text (dateFormat ++ " - " ++ timeFormat)
        ]


fullDateFormatter : SharedState -> Posix -> Html msg
fullDateFormatter sharedState time =
    let
        weekDayType =
            Maybe.withDefault Time.utc sharedState.timezone
                |> flip Time.toWeekday time

        weekday =
            dayFormatter sharedState weekDayType

        day =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone
                        |> flip Time.toDay time
                    )

        monthType =
            Maybe.withDefault Time.utc sharedState.timezone
                |> flip Time.toMonth time

        month =
            monthFormatter sharedState monthType

        year =
            String.fromInt
                (Maybe.withDefault Time.utc sharedState.timezone
                    |> flip Time.toYear time
                )
    in
    text <| String.toUpper (weekday ++ ", " ++ month ++ " " ++ day ++ ", " ++ year)


joinDateAndTime : SharedState -> Date -> TimePicker.Time -> Maybe Posix
joinDateAndTime sharedState date time =
    let
        resultDatePosix = dateToPosix date

        timeMillis = (time.seconds + (time.minutes * 60) + (time.hours * 60 * 60)) * 1000
    in
    case resultDatePosix of
        Ok datePosix -> Just <| Time.millisToPosix <| timeMillis + (Time.posixToMillis datePosix)

        Err _ -> Nothing

timeZoneToUtcOffsetMinutes : Time.Zone -> Int
timeZoneToUtcOffsetMinutes zone =
    let
       hourOffset = Time.toHour zone <| Time.millisToPosix 0
       minuteOffset = Time.toMinute zone <| Time.millisToPosix 0

       _ = Debug.log "Time" (hourOffset, minuteOffset)
    in
    0


utcOffsetMinutes : Int -> Int -> Int -> Int
utcOffsetMinutes multiplier hours minutes =
    -- multiplier is either 1 or -1 (for negative UTC offsets)
    multiplier * ((hours * 60) + minutes)

