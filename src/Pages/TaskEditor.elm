module Pages.TaskEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Task exposing (Task)
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
    { id : Int
    , max_points : String
    , task : WebData Task
    }


initCreate : ( Model, Cmd Msg )
initCreate =
    ( { id = 0
      , max_points = ""
      , task = NotAsked
      }
    , Cmd.none
    )


initEdit : Int -> ( Model, Cmd Msg )
initEdit id =
    ( { id = id
      , max_points = ""
      , task = Loading
      }
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
