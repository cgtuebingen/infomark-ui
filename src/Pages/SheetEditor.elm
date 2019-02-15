module Pages.SheetEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

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


initCreate : ( Model, Cmd Msg )
initCreate =
    ( { dummy = 0 }, Cmd.none )


initEdit : Int -> ( Model, Cmd Msg )
initEdit id =
    ( { dummy = 0 }, Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
