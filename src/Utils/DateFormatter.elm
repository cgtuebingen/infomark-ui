module Utils.DateFormatter exposing
    ( dateAndTimeFormatter
    , dateToShortFormatString
    , dayFormatter
    , fullDateFormatter
    , monthFormatter
    , shortDateFormatter
    , shortDayFormatter
    , timeFormatter
    )

import Date exposing (Date)
import Html exposing (..)
import I18n
import Iso8601
import SharedState exposing (SharedState)
import Time exposing (Posix, Zone(..))
import Utils.DateAndTimeUtils exposing (dateToPosix)
import Utils.Utils exposing (flip)


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
