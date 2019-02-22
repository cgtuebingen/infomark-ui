module Components.Toasty exposing (Toast(..), config, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Toasty


{-| This theme defines toasts of three variants: "Success", "Warning" and "Error".
Each of them accepts a title and an optional secondary text.
-}
type Toast
    = Success String String
    | Warning String String
    | Error String String


{-| Default theme configuration.
-}
config : Toasty.Config msg
config =
    Toasty.config
        |> Toasty.transitionOutDuration 700
        |> Toasty.transitionOutAttrs transitionOutAttrs
        |> Toasty.transitionInAttrs transitionInAttrs
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.itemAttrs itemAttrs
        |> Toasty.delay 5000


containerAttrs : List (Html.Attribute msg)
containerAttrs =
    [ classes
        [ TC.fixed
        , TC.top_0
        , TC.right_0
        , TC.w_100
        , TC.measure
        , TC.list
        , TC.pa0
        , TC.ma0
        ]
    ]


itemAttrs : List (Html.Attribute msg)
itemAttrs =
    [ classes
        [ TC.mt3
        , TC.mr3
        , TC.mb0
        , TC.ml3
        , TC.mh4
        ]
    ]


transitionInAttrs : List (Html.Attribute msg)
transitionInAttrs =
    [ class "animated bounceInRight"
    ]


transitionOutAttrs : List (Html.Attribute msg)
transitionOutAttrs =
    [ class "animated fadeOutRightBig"
    , style "max-height" "0"
    , style "margin-top" "0"
    ]


{-| Default theme view handling the three toast variants.
-}
view : Toast -> Html msg
view toast =
    case toast of
        Success title message ->
            genericToast toastySuccess title message

        Warning title message ->
            genericToast toastyWarning title message

        Error title message ->
            genericToast toastyFailure title message


toastySuccess =
    classes [ TC.bg_dark_green, TC.white ]


toastyFailure =
    classes [ TC.bg_red, TC.white ]


toastyWarning =
    classes [ TC.bg_orange, TC.black ]


genericToast : Html.Attribute msg -> String -> String -> Html msg
genericToast variantStyle title message =
    div
        [ classes
            [ TC.pa3
            , TC.br3
            , TC.pointer
            , TC.shadow_5
            , TC.f6
            ]
        , variantStyle
        ]
        [ h1 [ classes [ TC.f5, TC.ma0 ] ] [ text title ]
        , if String.isEmpty message then
            text ""

          else
            p [ classes [ TC.f6, TC.mt2, TC.mt0 ] ] [ text message ]
        ]
