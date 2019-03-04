module Components.CommonElements exposing
    ( dateInputElement
    , inputElement
    , sliderInputElement
    , textAreaElement
    , timeInputElement
    , viewFormErrors
    )

import Date exposing (Date)
import DatePicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import TimePicker exposing (TimeEvent(..), TimePicker)
import Utils.Styles as Styles


inputElement : { label : String, placeholder : String, fieldType : String, value : String } -> field -> List ( field, String ) -> (field -> String -> msg) -> List (Html msg)
inputElement inputConfig field errors msg =
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputConfig.label
        ]
    , input
        [ type_ inputConfig.fieldType
        , Styles.lineInputStyle
        , classes [ TC.w_100, TC.mb3 ]
        , placeholder inputConfig.placeholder
        , onInput <| msg field
        , value inputConfig.value
        ]
        []
    , viewFormErrors field errors
    ]


viewFormErrors : field -> List ( field, String ) -> Html msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [ TC.red ] ] [ text error ])
        |> ul [ classes [ TC.list, TC.pl0, TC.center ] ]


dateInputElement :
    { label : String
    , value : Maybe Date
    , datePicker : DatePicker.DatePicker
    , settings : DatePicker.Settings
    }
    -> field
    -> List ( field, String )
    -> (DatePicker.Msg -> msg)
    -> List (Html msg)
dateInputElement inputConfig field errors msg =
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputConfig.label ]
    , DatePicker.view inputConfig.value inputConfig.settings inputConfig.datePicker
        |> Html.map msg
    , viewFormErrors field errors
    ]


timeInputElement :
    { label : String
    , placeholder : String
    , timePicker : TimePicker
    , settings : TimePicker.Settings
    }
    -> field
    -> List ( field, String )
    -> (TimePicker.Msg -> msg)
    -> List (Html msg)
timeInputElement inputConfig field errors msg =
    let
        originalConfig =
            inputConfig.settings

        updatedConfig =
            { originalConfig | placeholder = inputConfig.placeholder }
    in
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputConfig.label ]
    , div [ class "default-time-picker" ]
        [ Html.map msg <|
            TimePicker.view
                updatedConfig
                inputConfig.timePicker
        ]
    , div [ classes [ TC.mt4, TC.pt2 ] ] [ viewFormErrors field errors ]
    ]


textAreaElement : { label : String, placeholder : String, value : String } -> field -> List ( field, String ) -> (field -> String -> msg) -> List (Html msg)
textAreaElement inputConfig field errors msg =
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputConfig.label
        ]
    , textarea
        ([ Styles.lineInputStyle
         , classes [ TC.w_100, TC.mb3 ]
         , placeholder inputConfig.placeholder
         , onInput <| msg field
         , value inputConfig.value
         ]
            ++ Styles.textAreaReset
        )
        []
    , viewFormErrors field errors
    ]


sliderInputElement : { label : String, value : Int, min : Int, max : Int, step : Int, valueLabel : String } -> field -> List ( field, String ) -> (field -> String -> msg) -> List (Html msg)
sliderInputElement inputConfig field errors msg =
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputConfig.label
        ]
    , input
        [ type_ "range"
        , Html.Attributes.min <| String.fromInt inputConfig.min
        , Html.Attributes.max <| String.fromInt inputConfig.max
        , value <| String.fromInt inputConfig.value
        , step <| String.fromInt inputConfig.step
        , class "slider"
        , onInput <| msg field
        , classes [ TC.mt3, TC.bg_black_30 ]
        ]
        []
    , h2 [ Styles.labelStyle ] [ text inputConfig.valueLabel ]
    , viewFormErrors field errors
    ]
