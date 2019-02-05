{-
    This is the home site with links to all courses.
    If the user is not logged in redirect to the login page
-}
module Pages.Home exposing (..)

import Html exposing (..)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { logged_in : Bool
    }


type Msg
    = CheckLogin
    | NavigateTo Route
     

init : ( Model, Cmd Msg)
init =
    ( { logged_in = False
      }
    , Cmd.none
    )

view : SharedState -> Model -> Html Msg
view sharedState model = div [] []