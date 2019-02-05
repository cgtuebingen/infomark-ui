{-
    This is the home site with links to all courses.
    If the user is not logged in redirect to the login page
-}
module Pages.Home exposing (..)

import Routing.Helpers exposing (Route(..), reverseRoute)


type alias Model =
    { username : String
    , plain_password : String
    }


type Msg
    = CheckLogin
    | NavigateTo Route
     

init : ( Model, Cmd Msg)
init =
    ( { username = ""
      , plain_password = ""
      }
    , Cmd.none
    )