{-
   This is the login site. Here, tutors and students should get
   an overview about what to do next.
       - Users should see:
           - All enrolled courses
       - Students should see (per course):
           - An overview of their points for each sheet
           - The total amount of points acquired
           - The needed amount of points to pass the course
           - What tasks are missing
       - Tutors should see (per course):
           - The point distribution per exercise sheet of the course or group?
           - The tasks which are not done grading
-}


module Pages.Dashboard exposing (Model, Msg(..), init, update, view)

import Api.Data.Course exposing (Course)
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Utils.Styles as Styles


type Msg
    = NavigateTo Route


type alias Model =
    { dummy : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { dummy = 0 }, Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
