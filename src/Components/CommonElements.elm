module Components.CommonElements exposing
    ( inputLabel
    , dateInputElement
    , inputElement
    , sliderInputElement
    , textAreaElement
    , timeInputElement
    , viewFormErrors
    , fileUploader
    , pageContainer
    , widePage
    , normalPage
    , rContainer
    , rRow
    , rRowHeader
    , rRowButton
    , rRowHeaderActionButtons
    , rRowLabelButton
    , rRowExtraSpacing
    , rCollapsable
    , r1Column
    , r2Column
    , r3Column
    )

import Date exposing (Date)
import DatePicker
import File exposing (File)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import TimePicker exposing (TimeEvent(..), TimePicker)
import Utils.Styles as Styles


inputElement : { label : String, placeholder : String, fieldType : String, value : String } -> field -> List ( field, String ) -> (field -> String -> msg) -> List (Html msg)
inputElement inputConfig field errors msg =
    [ inputLabel inputConfig.label
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
    [ inputLabel inputConfig.label 
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
    [ inputLabel inputConfig.label
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
    [ inputLabel inputConfig.label
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
    [ inputLabel inputConfig.label
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


inputLabel : String -> Html msg
inputLabel title =
    label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text title
        ]


pageContainer : List (Html msg) -> Html msg
pageContainer childs =
    div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0_ns, TC.w_100 ] ]
        childs


widePage : List (Html msg) -> Html msg
widePage childs =
    div [ classes [ TC.w_75_l, TC.w_100, TC.ph5, TC.ph0_l, TC.center, TC.mw9_ns ] ]
        childs


normalPage : List (Html msg) -> Html msg
normalPage childs =
    div [ classes [ TC.mw8, TC.ph4, TC.ph5_ns, TC.center ] ] 
        childs


rContainer : List (Html msg) -> Html msg
rContainer childs =
    div [ classes [ TC.w_100 ] ]
        childs


rRow : List (Html msg) -> Html msg
rRow childs =
    div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
        childs


rRowExtraSpacing : List (Html msg) -> Html msg
rRowExtraSpacing childs = 
    div [ classes [ TC.mt3, TC.mt4_ns, TC.cf, TC.ph2_ns ] ]
        childs


rRowHeader : String -> Html msg
rRowHeader label =
    rRow <|
        [ h1 [ Styles.headerStyle ] [ text label ] ]


rRowHeaderActionButtons : String -> List (String, msg, Html.Attribute msg) -> Html msg
rRowHeaderActionButtons label actions =
    div [ 
        classes 
            [ TC.flex
            , TC.flex_row
            , TC.justify_between
            , TC.items_center
            , TC.bb
            , TC.bw2 
            ] 
        ] <| 
        h1 [ Styles.headerStyle ] [ text label ] :: 
            List.map (\(actionLabel, actionMsg, baseStyle) ->
                button
                    [ baseStyle
                    , classes [ TC.br_pill, TC.ph4, TC.pv3 ]
                    , onClick actionMsg
                    ] [ text actionLabel ]
            ) actions
    


rRowButton : String -> msg -> Bool -> Html msg
rRowButton buttonText msg enabled =
    div [ classes [ TC.mt3, TC.cf, TC.ph4_ns, TC.ph3 ] ]
        [ button
            (if enabled then
                [ Styles.buttonGreyStyle
                , classes [ TC.mb4, TC.mt3, TC.w_100 ]
                , onClick msg
                ]
            else
                [ Styles.buttonDisabled ]
            )
            [ text buttonText ]
        ]

rRowLabelButton : String -> String -> msg -> Html msg
rRowLabelButton label buttonText msg =
    div [ classes
            [ TC.w_100
            , TC.flex
            , TC.flex_row
            , TC.justify_between
            , TC.items_center
            ]
    ]
    [ h1 [ Styles.listHeadingStyle] [ text label ]
    , button 
        [ Styles.buttonGreyStyle
        , Styles.pillStyle 
        , onClick msg
        ]
        [ text buttonText ]
    ]


rCollapsable : String -> Bool -> msg -> (String, String) -> List (Html msg) -> List (Html msg)
rCollapsable title collapsed collapseMsg (show, hide) childs =
    div [ classes
            [ TC.w_100
            , TC.flex
            , TC.flex_row
            , TC.justify_between
            , TC.items_center
            , if collapsed then TC.mb3 else TC.mb0
            ]
        ] 
        [ h1 [ Styles.listHeadingStyle ] [ text title ]
        , button
            [ Styles.buttonGreyStyle
            , Styles.pillStyle
            , onClick collapseMsg
            ]
            [ text <| if collapsed then show else hide ]
        ] :: if collapsed then [ text "" ] else childs
    

r2Column : List (Html msg) -> List (Html msg) -> List (Html msg)
r2Column child1 child2 =
    [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ]
        child1
    , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
        child2
    ]


r1Column : List (Html msg) -> List (Html msg)
r1Column child =
    child


r3Column : List (Html msg) -> List (Html msg) -> List (Html msg) -> List (Html msg)
r3Column child1 child2 child3 =
    [ div [ classes [ TC.fl, TC.w_100, TC.w_50_m, TC.w_33_l ] ]
        child1
    , div [ classes [ TC.fl, TC.w_100, TC.w_50_m, TC.w_33_l, TC.pl2_ns ] ]
        child2
    , div [ classes [ TC.fl, TC.w_100, TC.w_50_m, TC.w_33_l, TC.pl2_ns ] ]
        child3
    ]


--Same for image uploader?
fileUploader : Bool -> Maybe File -> msg -> msg -> msg -> (File -> List File -> msg) -> Html msg
fileUploader hover file enterMsg exitMsg pickMsg gotFileMsg =
    div
        [ classes
            [ TC.pa4
            , TC.ba
            , TC.b__dashed
            , if hover then
                TC.b__dark_red
              else
                TC.b__black_40
            , TC.bw2
            , TC.br3
            , TC.w_100
            , TC.flex
            , TC.flex_column
            , TC.justify_center
            , TC.items_center
            , TC.fl
            ]
        , hijackOn "dragenter" (Decode.succeed <| enterMsg)
        , hijackOn "dragover" (Decode.succeed <| enterMsg)
        , hijackOn "dragleave" (Decode.succeed <| exitMsg)
        , hijackOn "drop" (dropDecoder gotFileMsg)
        ]
        [ span
            [ Styles.labelStyle
            ]
            [ text <| Maybe.withDefault "" <| Maybe.map File.name <| file ]
        , button
            [ Styles.buttonGreyStyle
            , classes
                [ TC.w_100
                , TC.mt4
                ]
            , onClick pickMsg
            ]
            [ text "Pick file" ]
        ]


dropDecoder : (File -> List File -> msg) -> Decoder msg
dropDecoder msg =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore msg File.decoder)


hijackOn : String -> Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )