module Pages.Registration exposing (..)

import Routing.Helpers exposing (Route(..), reverseRoute)


type alias Model =
    { username : String
    , plain_password : String
    }

type Msg
    = NavigateTo Route

initModel : Model
initModel =
    { username = ""
    , plain_password = ""
    }