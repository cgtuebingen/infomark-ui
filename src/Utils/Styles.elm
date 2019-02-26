module Utils.Styles exposing
    ( buttonBlackStyle
    , buttonGoldStyle
    , buttonGreenStyle
    , buttonGreyStyle
    , buttonRedStyle
    , dateStyle
    , dialogContainerStyle
    , dialogGoneStyle
    , dialogOverlayStyle
    , dialogVisibleStyle
    , headerStyle
    , inputStyle
    , labelStyle
    , lineInputStyle
    , linkBlackStyle
    , linkGoldStyle
    , linkGreenStyle
    , linkGreyStyle
    , linkRedStyle
    , linkWhiteStyle
    , listHeadingStyle
    , sectionStyle
    , spinnerGoldStyle
    , spinnerGreyStyle
    , spinnerRedStyle
    , textAreaReset
    , textStyle
    )

import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Spinner
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC


textAreaReset : List (Html.Attribute msg)
textAreaReset =
    [ style "resize" "none"

    -- , style "border" "none"
    , style "outline" "none"
    ]


linkStyle =
    [ TC.f5
    , TC.dib
    , TC.ph2
    , TC.link
    , TC.dim
    , TC.pointer
    , TC.button_reset ---Everything for applying the style to a button instead of "a"
    , TC.input_reset
    , TC.bg_transparent
    , TC.hover_bg_transparent
    , TC.bn
    , TC.no_underline
    ]


linkBlackStyle =
    classes <|
        linkStyle
            ++ [ TC.black ]


linkWhiteStyle =
    classes <|
        linkStyle
            ++ [ TC.white ]


linkRedStyle =
    classes <|
        linkStyle
            ++ [ TC.dark_red ]


linkGoldStyle =
    classes <|
        linkStyle
            ++ [ TC.gold ]


linkGreyStyle =
    classes <|
        linkStyle
            ++ [ TC.dark_gray ]


linkGreenStyle =
    classes <|
        linkStyle
            ++ [ TC.dark_green ]


buttonStyle =
    [ TC.f5
    , TC.no_underline
    , TC.bg_animate
    , TC.dib
    , TC.b
    , TC.items_center
    , TC.pa3
    , TC.ba
    , TC.border_box
    , TC.bg_white
    , TC.pointer
    , TC.button_reset
    , TC.input_reset
    ]


buttonBlackStyle =
    classes <|
        buttonStyle
            ++ [ TC.black
               , TC.b__black
               , TC.hover_bg_black
               , TC.hover_white
               ]


buttonRedStyle =
    classes <|
        buttonStyle
            ++ [ TC.dark_red
               , TC.b__dark_red
               , TC.hover_bg_dark_red
               , TC.hover_white
               ]


buttonGoldStyle =
    classes <|
        buttonStyle
            ++ [ TC.gold
               , TC.b__gold
               , TC.hover_bg_gold
               , TC.hover_black
               ]


buttonGreyStyle =
    classes <|
        buttonStyle
            ++ [ TC.dark_gray
               , TC.b__dark_gray
               , TC.hover_bg_dark_gray
               , TC.hover_white
               ]


buttonGreenStyle =
    classes <|
        buttonStyle
            ++ [ TC.dark_green
               , TC.b__dark_green
               , TC.hover_bg_dark_green
               , TC.hover_white
               ]


inputStyle =
    classes
        [ TC.pa3
        , TC.input_reset
        , TC.ba
        , TC.bg_transparent
        , TC.hover_bg_dark_gray
        , TC.hover_white
        , TC.dark_gray
        , TC.f5
        ]


lineInputStyle : Html.Attribute msg
lineInputStyle =
    classes
        [ TC.bb
        , TC.bw1
        , TC.bt_0
        , TC.bl_0
        , TC.br_0
        , TC.ph0
        , TC.pv3
        , TC.input_reset
        , TC.b__dark_red
        , TC.black_80
        , TC.f5
        ]


headerStyle =
    classes
        [ TC.f1
        , TC.fw8
        , TC.black
        , TC.b
        ]

sectionStyle =
    classes
        [ TC.f2
        , TC.fw6
        , TC.black
        , TC.b
        , TC.lh_title
        ]

listHeadingStyle =
    classes
        [ TC.f3, TC.fw6, TC.lh_title, TC.black ]


labelStyle =
    classes
        [ TC.f5
        , TC.fw6
        , TC.black_80
        ]


textStyle =
    classes
        [ TC.f5
        , TC.black_80
        , TC.lh_copy
        ]


dateStyle =
    classes
        [ TC.f5
        , TC.ttu
        , TC.tracked
        , TC.dark_gray
        , TC.lh_copy
        ]


dialogOverlayStyle =
    classes
        [ TC.fixed
        , TC.top_0
        , TC.right_0
        , TC.bottom_0
        , TC.left_0
        , TC.bg_black_30
        , TC.justify_center
        , TC.items_center
        ]


dialogContainerStyle =
    classes
        [ TC.bg_white
        , TC.black
        , TC.ba
        , TC.bw1
        , TC.b__black_60
        , TC.shadow_5
        , TC.w_100
        , TC.measure_wide
        , TC.ph5
        , TC.pv4
        ]


dialogVisibleStyle =
    classes [ TC.flex ]


dialogGoneStyle =
    classes [ TC.dn ]


spinnerStyle =
    { lines = 10
    , length = 0
    , width = 8
    , radius = 35
    , scale = 0.5
    , corners = 1
    , opacity = 0.25
    , rotate = 0
    , direction = Spinner.Clockwise
    , speed = 1
    , trail = 100
    , translateX = 50
    , translateY = 50
    , shadow = True
    , hwaccel = False
    , color = always <| Color.rgba 1 1 1 1
    }


spinnerRedStyle =
    { spinnerStyle | color = always <| Color.rgba (165 / 255) (30 / 255) (55 / 255) 1 }


spinnerGoldStyle =
    { spinnerStyle | color = always <| Color.rgba (180 / 255) (160 / 255) (105 / 255) 1 }


spinnerGreyStyle =
    { spinnerStyle | color = always <| Color.rgba (50 / 255) (65 / 255) (75 / 255) 1 }
