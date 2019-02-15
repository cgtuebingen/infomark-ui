{-
    Additional statistics about:
        - Readiness for work of tutors
            - How many missing graded submissions
        - Readiness for work of students
            - How many students stopped at submitting for sheet x
-}

module Pages.Admin exposing (..)

import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import RemoteData exposing (RemoteData(..), WebData)
import Utils.Styles as Styles
import Time
import I18n
import Api.Data.Course exposing (Course)


type Msg
    = NavigateTo Route


type alias Model =
    { dummy : Int

    }


init : (Model, Cmd Msg)
init = ({ dummy = 0 }, Cmd.none)

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        NavigateTo route ->
            (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Html Msg
view sharedState model = div [] []