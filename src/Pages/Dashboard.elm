{-
   This is the login site. Here, tutors and TaskForStudents should get
   an overview about what to do next.
       - Users should see:
           - All enrolled courses
       - TaskForStudents should see (per course):
           - An overview of their points for each sheet
           - The total amount of points acquired
           - The needed amount of points to pass the course
           - What tasks are missing
       - Tutors should see (per course):
           - The point distribution per exercise sheet of the course or group?
           - The tasks which are not done grading
-}


module Pages.Dashboard exposing (Model, Msg(..), init, update, view)

import Api.Data.AccountEnrollment exposing (AccountEnrollment)
import Api.Data.Course exposing (Course)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.MissingGrade exposing (MissingGrade)
import Api.Data.MissingTask exposing (MissingTask)
import Api.Request.Account as AccountRequests
import Api.Request.Courses as CourseRequests
import Browser.Navigation exposing (pushUrl)
import Dict exposing (Dict)
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
import Utils.Utils exposing (perform)


type Msg
    = NavigateTo Route
    | GetCourses (WebData (List Course))
    | GetAccountEnrollments (WebData (List AccountEnrollment))
    | GetMissingItem Int TodoRequest


type TodoRequest
    = TaskRequest (WebData (List MissingTask))
    | GradeRequest (WebData (List MissingGrade))


type TodoItems
    = TaskTodo (List MissingTask)
    | GradeTodo (List MissingGrade)


type alias FusedEnrollment =
    { course : Course
    , role : CourseRole
    , missingItems : Maybe TodoItems
    }


type alias Model =
    { getCourses : WebData (List Course)
    , getAccountEnrollments : WebData (List AccountEnrollment)
    , fusedEnrollmentDict : Dict Int FusedEnrollment
    }


init : SharedState -> ( Model, Cmd Msg )
init sharedState =
    let
        startRequests =
            Cmd.batch
                [ CourseRequests.coursesGet GetCourses
                , AccountRequests.accountEnrollmentGet GetAccountEnrollments
                ]
    in
    ( { getCourses = Loading
      , getAccountEnrollments = Loading
      , fusedEnrollmentDict = Dict.empty
      }
    , case ( sharedState.userMail, sharedState.role ) of
        ( Just _, Just role ) ->
            if role.root then
                perform <| NavigateTo CoursesRoute

            else
                startRequests

        ( _, _ ) ->
            {- This one includes even lost userMail. Here, a request is started
               which will fail and trigger a login refresh dialog. If no usermail is
               left in the persistant or sharedstate then the user is redirected to
               the login form
            -}
            startRequests
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        GetCourses response ->
            fillFusedEnrollmentAndDecideRequests
                { model | getCourses = response }

        GetAccountEnrollments response ->
            fillFusedEnrollmentAndDecideRequests
                { model | getAccountEnrollments = response }

        GetMissingItem courseId response ->
            ( updateFusedEnrollmentWithTodos model courseId response
            , Cmd.none
            , NoUpdate
            )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []


updateFusedEnrollmentWithTodos : Model -> Int -> TodoRequest -> Model
updateFusedEnrollmentWithTodos model courseId todoRequest =
    (case todoRequest of
        TaskRequest response ->
            RemoteData.map (\mts -> TaskTodo mts) response

        GradeRequest response ->
            RemoteData.map (\mgs -> GradeTodo mgs) response
    )
        |> RemoteData.toMaybe
        |> (\maybeTodos ->
                { model
                    | fusedEnrollmentDict =
                        Dict.update courseId
                            (Maybe.map (\fe -> { fe | missingItems = maybeTodos }))
                            model.fusedEnrollmentDict
                }
           )


fillFusedEnrollmentAndDecideRequests : Model -> ( Model, Cmd Msg, SharedStateUpdate )
fillFusedEnrollmentAndDecideRequests model =
    let
        nextRequests =
            \m ->
                case ( m.getCourses, m.getAccountEnrollments ) of
                    ( Success _, Success _ ) ->
                        m.fusedEnrollmentDict
                            |> Dict.values
                            |> List.map
                                (\fe ->
                                    case fe.role of
                                        Student ->
                                            Cmd.map (GetMissingItem fe.course.id) <|
                                                CourseRequests.courseTaskMissing
                                                    fe.course.id
                                                    TaskRequest

                                        Tutor ->
                                            Cmd.map
                                                (GetMissingItem fe.course.id)
                                            <|
                                                CourseRequests.courseGradeMissing
                                                    fe.course.id
                                                    GradeRequest

                                        Admin ->
                                            Cmd.none
                                )
                            |> Cmd.batch

                    _ ->
                        Cmd.none
    in
    (case ( model.getCourses, model.getAccountEnrollments ) of
        ( Success courses, Success enrollments ) ->
            enrollments
                |> List.map
                    (\e ->
                        courses
                            |> List.filter (\c -> c.id == e.course_id)
                            |> List.head
                            |> Maybe.map
                                (\c ->
                                    { course = c
                                    , role = e.role
                                    , missingItems = Nothing
                                    }
                                )
                    )
                |> List.filterMap identity
                |> List.map (\fe -> ( fe.course.id, fe ))
                |> Dict.fromList
                |> (\fe -> { model | fusedEnrollmentDict = fe })

        ( _, _ ) ->
            model
    )
        |> (\m -> ( m, nextRequests m, NoUpdate ))
