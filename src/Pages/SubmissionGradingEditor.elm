{-
   Grade submission (for tutors)
-}


module Pages.SubmissionGradingEditor exposing (Model, Msg(..), init, update, view)

import Api.Data.Course exposing (Course)
import Api.Data.Grade exposing (Grade)
import Api.Data.Task exposing (Task)
import Api.Data.User exposing (User)
import Api.Endpoint exposing (submissionFile)
import Api.Request.Courses as CourseRequests
import Api.Request.Task as TaskRequests
import Api.Request.User as UserRequests
import Browser.Navigation exposing (pushUrl)
import Components.UserAvatarEmailView as UserView
import Dict exposing (Dict)
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
    | GetTask (WebData Task)
    | GetUserInfo Int (WebData User)


type alias Model =
    { getGradesResponse : WebData (List Grade)
    , getTaskResponse : WebData Task
    , getUserResponses : Dict Int (WebData User)
    , fusedGradeDict : Dict Int FusedGrade
    }


type alias FusedGrade =
    { grade : Grade
    , user : User
    }


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init courseId taskId groupId =
    ( { getGradesResponse = Loading
      , getTaskResponse = Loading
      , getUserResponses = Dict.empty
      , fusedGradeDict = Dict.empty
      }
    , Cmd.batch
        [ CourseRequests.courseGradesGetPerTaskAndGroup courseId taskId groupId GetGrades
        , TaskRequests.taskGet courseId taskId GetTask
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )

        GetGrades response ->
            ( { model | getGradesResponse = response }, Cmd.none, NoUpdate )

        GetTask response ->
            ( { model | getTaskResponse = response }, Cmd.none, NoUpdate )

        GetUserInfo gradeId response ->
            ( { model
                | getUserResponses = Dict.insert gradeId response model.getUserResponses
              }
            , Cmd.none
            , NoUpdate
            )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
