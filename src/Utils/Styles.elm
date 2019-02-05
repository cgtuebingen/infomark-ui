module Utils.Styles exposing (linkBlackStyle, linkRedStyle, linkGoldStyle, linkGreyStyle, 
                              buttonBlackStyle, buttonRedStyle, buttonGoldStyle, buttonGreyStyle,
                              inputStyle)

import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC


linkStyle =  
    [ TC.f6 
    , TC.dib 
    , TC.ph2 
    , TC.link
    , TC.dim 
    ]

linkBlackStyle = classes <|
    linkStyle ++
    [ TC.black ]

linkRedStyle = classes <|
    linkStyle ++
    [ TC.dark_red ]

linkGoldStyle = classes <|
    linkStyle ++
    [ TC.gold ]

linkGreyStyle = classes <|
    linkStyle ++
    [ TC.dark_gray ]


buttonStyle =
    [ TC.f5 
    , TC.no_underline
    , TC.bg_animate
    , TC.inline_flex 
    , TC.items_center 
    , TC.pa3 
    , TC.ba 
    , TC.border_box
    , TC.bg_white
    , TC.pointer
    , TC.button_reset
    ]


buttonBlackStyle = classes <|
    buttonStyle ++
    [ TC.black 
    , TC.b__black
    , TC.hover_bg_black 
    , TC.hover_white 
    ]


buttonRedStyle = classes <|
    buttonStyle ++
    [ TC.dark_red 
    , TC.b__dark_red
    , TC.hover_bg_dark_red
    , TC.hover_white 
    ]

buttonGoldStyle = classes <|
    buttonStyle ++
    [ TC.gold 
    , TC.b__gold
    , TC.hover_bg_gold
    , TC.hover_black
    ]

buttonGreyStyle = classes <|
    buttonStyle ++
    [ TC.dark_gray
    , TC.b__dark_gray
    , TC.hover_bg_dark_gray
    , TC.hover_white 
    ]


inputStyle = classes
    [ TC.pa3
    , TC.input_reset 
    , TC.ba 
    , TC.bg_transparent 
    , TC.hover_bg_dark_red
    , TC.hover_white
    , TC.black_80
    ]
