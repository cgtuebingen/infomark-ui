module Utils.Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

uniRed = "#A51E37"
uniGrey = "#324148"
uniGold = "#B4A069"
warningRed = "#A94442"
successGreen = "#137752"
white = "#FFFFFF"
black = "#000000"

warning : String -> Svg msg
warning color = Svg.path 
    [ d "M13,13H11V7H13M13,17H11V15H13M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2Z"
    , fill color] []