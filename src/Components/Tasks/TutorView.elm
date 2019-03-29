module Components.Tasks.TutorView exposing (Model, Msg(..), init, update, view)

import Api.Data.Group exposing (Group)
import Api.Data.Task exposing (Task)
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements as CE
import Html exposing (..)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles


type Msg
    = NavigateTo Route
    | NoOp


type alias Model =
    { id : Int
    , courseId : Int
    , task : Task
    , ownGroups : List Group
    }


init : Int -> Task -> List Group -> ( Model, Cmd Msg )
init courseId task ownGroups =
    ( { id = task.id
      , courseId = courseId
      , task = task
      , ownGroups = ownGroups
      }
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
