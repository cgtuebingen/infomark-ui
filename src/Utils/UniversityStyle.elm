module Utils.UniversityStyle exposing (..)

import Html exposing (Attribute)
import Html.Styled exposing (..)
import Css exposing (..)

type alias BackgroundTextColor = { background : Color, text : Color }

theme : { primary : BackgroundTextColor, secondary : BackgroundTextColor, tertiary : BackgroundTextColor }
theme =
    { primary = { background = hex "324148", text = hex "FFFFFF" }
    , secondary = { background = hex "A51E37", text = hex "FFFFFF" }
    , tertiary = { background = hex "B4A069", text = hex "000000" }
    }

state : { success : BackgroundTextColor, failure : BackgroundTextColor }
state = 
    { success = { background = hex "2E7D32", text = hex "FFFFFF" }
    , failure = { background = hex "A94442", text = hex "FFFFFF" }
    }
