module Utils.DateFormatter exposing (dateAndTimeFormatter, fullDateFormatter, shortDateFormatter, timeFormatter)

import Html exposing (..)
import I18n
import SharedState exposing (SharedState)
import Time exposing (Posix)


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b

timeFormatter : SharedState -> Posix -> Html msg
timeFormatter sharedState time =
    let
        hour =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toHour time)

        minute =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toMinute time)

        second =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toSecond time)
    in
    text (hour ++ ":" ++ minute ++ ":" ++ second)


shortDateFormatter : SharedState -> Posix -> Html msg
shortDateFormatter sharedState time =
    let
        t =
            I18n.get sharedState.translations

        day =
            String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toDay time)

        monthType = Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toMonth time

        month =
            case monthType of
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

        year =
            String.fromInt
                (Maybe.withDefault Time.utc sharedState.timezone |>
                    flip Time.toYear time)
    in
    text (day ++ "/" ++ month ++ "/" ++ year)


dateAndTimeFormatter : SharedState -> Posix -> Html msg
dateAndTimeFormatter sharedState time =
    let
        timeFormat =
            timeFormatter sharedState time

        dateFormat =
            shortDateFormatter sharedState time
    in
    span []
        [ dateFormat
        , text " - "
        , timeFormat
        ]


fullDateFormatter : SharedState -> Posix -> Html msg
fullDateFormatter sharedState time =
    let
        t =
            I18n.get sharedState.translations

        weekDayType = Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toWeekday time

        weekday =
            case weekDayType of
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

        day = String.padLeft 2 '0' <|
                String.fromInt
                    (Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toDay time)

        monthType = Maybe.withDefault Time.utc sharedState.timezone |>
                        flip Time.toMonth time

        month =
            case monthType of
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

        year =
            String.fromInt
                (Maybe.withDefault Time.utc sharedState.timezone |>
                    flip Time.toYear time)
    in
    text <| String.toUpper (weekday ++ ", " ++ month ++ " " ++ day ++ ", " ++ year)
