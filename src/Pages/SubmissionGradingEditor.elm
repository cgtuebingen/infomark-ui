{-
   Grade submission (for tutors)
-}


module Pages.SubmissionGradingEditor exposing (Model, Msg(..), init, update, view)

import Api.Data.Course exposing (Course)
import Api.Data.Grade exposing (Grade)
import Api.Endpoint exposing (submissionFile)
import Api.Request.Courses as CourseRequests
import Browser.Navigation exposing (pushUrl)
import File.Download as Download
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
    | GetGrades (WebData (List Grade))


type alias Model =
    { getGradesResponse : WebData (List Grade)
    }


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init courseId taskId groupId =
    ( { getGradesResponse = Loading
      }
    , CourseRequests.courseGradesGetPerTaskAndGroup courseId taskId groupId GetGrades
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )

        GetGrades response ->
            ( { model | getGradesResponse = response }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
