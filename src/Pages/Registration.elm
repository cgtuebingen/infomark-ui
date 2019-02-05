module Pages.Registration exposing (..)

import Html exposing (..)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))

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

view : SharedState -> Model -> Html Msg
view sharedState model = div [] []