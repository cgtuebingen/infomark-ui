{-
   This is the sheet detail page. It displays:
       - Basic info such as name, dates, a warning if the submission is already closed
       - The link to download the PDF instructions
       - The tasks as a list - Maybe as cards:
           - If you are a student:
               - Option to rate the task and upload your submission
               - Small view about the test results
               - After ratings are finished show the feedback and points
           - If you are an root:
               - Option to create/edit/delete tasks
           - If you are a supertutor:
               - Link to the tasks create/editing view
           - If you are a tutor/supertutor:
               - Show the following per default for your own group/s.
                 Make it overrideable to other groups in case of ill tutors:
                   - Link to the grading view for each task after submission deadline
                   - Link to download all submissions for the task and sheet
       - Ratings/Reviews
           - If you are a student:
               - The gradings as a statistic of your sheet
           - If you are a root/tutor/supertutor
               - The students ratings as a statistic
-}


module Pages.SheetDetail exposing (Model, Msg(..), init, update, view)

import Api.Data.AccountEnrollment exposing (AccountEnrollment)
import Api.Data.CourseRole exposing (CourseRole(..))
import Api.Data.Sheet exposing (Sheet)
import Api.Data.Task exposing (Task)
import Api.Request.Account as AccountRequests
import Api.Request.Sheet as SheetRequests
import Browser.Navigation exposing (pushUrl)
import Components.TaskEditor as TaskEditor
import Components.TaskViewer as TaskViewer
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Time
import Utils.Styles as Styles
import Utils.Utils as Utils


type Msg
    = NavigateTo Route
    | AdminTaskMsg Int TaskEditor.Msg
    | StudentTaskMsg Int TaskViewer.Msg
    | GetTaskFetchResponse (WebData (List Task))
    | GetEnrollmentResponse (WebData (List AccountEnrollment))


type alias Model =
    { id : Int
    , course_id : Int
    , role : Maybe CourseRole
    , tasksAdmins : Dict Int TaskEditor.Model -- Only for admins
    , tasksStudents : Dict Int TaskViewer.Model -- Only for students
    }


init : Int -> Int -> ( Model, Cmd Msg )
init course_id id =
    ( { id = id
      , course_id = course_id
      , role = Nothing
      , tasksAdmins = Dict.empty
      , tasksStudents = Dict.empty
      }
    , Cmd.batch 
        [ SheetRequests.sheetTasksGet course_id id GetTaskFetchResponse
        , AccountRequests.accountEnrollmentGet GetEnrollmentResponse
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        GetTaskFetchResponse (Success tasks) ->
            ( fillModelTaskDict model tasks, Cmd.none, NoUpdate )

        GetTaskFetchResponse response ->
            ( model, Cmd.none, NoUpdate )

        GetEnrollmentResponse (Success roles) ->
            let
                maybeRole = roles |>
                    List.filter (\enrollment -> model.course_id == enrollment.course_id) |>
                    List.map (\enrollment -> enrollment.role) |>
                    List.head
            in
            case maybeRole of 
                Just role ->
                    ( { model | role = maybeRole }, Cmd.none, NoUpdate)
                
                Nothing ->
                    ( model, Utils.perform <| NavigateTo CoursesRoute, NoUpdate )

        GetEnrollmentResponse response ->
            ( model, Cmd.none, NoUpdate )

        AdminTaskMsg id subMsg ->
            case Dict.get id model.tasksAdmins of
                Just taskModel ->
                    let
                        ( newTaskModel, newTaskCmd, newTaskSharedState ) = 
                            TaskEditor.update sharedState subMsg taskModel
                    in
                    ({ model 
                        | tasksAdmins = Dict.update id (Maybe.map (\_ -> newTaskModel)) model.tasksAdmins }
                    , Cmd.map (AdminTaskMsg id) newTaskCmd
                    , newTaskSharedState
                    )
                    
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

        StudentTaskMsg id subMsg ->
            case Dict.get id model.tasksStudents of
                Just taskModel ->
                    let
                        ( newTaskModel, newTaskCmd, newTaskSharedState ) = 
                            TaskViewer.update sharedState subMsg taskModel
                    in
                    ({ model 
                        | tasksStudents = Dict.update id (Maybe.map (\_ -> newTaskModel)) model.tasksStudents }
                    , Cmd.map (StudentTaskMsg id) newTaskCmd
                    , newTaskSharedState
                    )
                    
                Nothing ->
                    ( model, Cmd.none, NoUpdate )


fillModelTaskDict : Model -> List Task -> Model
fillModelTaskDict model tasks =
    { model
        | tasksAdmins =
            tasks
                |> List.map (\task -> ( task.id, Tuple.first <| TaskEditor.initFromTask model.course_id task ))
                |> List.append [ ( -1, Tuple.first <| TaskEditor.initCreate model.course_id model.id ) ]
                |> Dict.fromList
        , tasksStudents =
            tasks
                |> List.map (\task -> ( task.id, Tuple.first <| TaskViewer.init model.course_id task ))
                |> Dict.fromList
    }


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0_ns, TC.w_100 ] ]
        [ --Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
          div
            [ classes
                [ TC.mw8
                , TC.ph4
                , TC.ph5_ns
                , TC.center
                ]
            ]
            [ case model.role of
                Just Admin -> viewTasksForAdmin sharedState model
                Just Student -> viewTasksForStudents sharedState model
                _ -> text ""
            ]
        ]


viewTasksForAdmin : SharedState -> Model -> Html Msg
viewTasksForAdmin sharedState model =
    div []
        (Dict.values model.tasksAdmins
            |> List.map (\task -> ( task.id, TaskEditor.view sharedState task ))
            |> List.map (\( id, task ) -> Html.map (AdminTaskMsg id) task)
        )

viewTasksForStudents : SharedState -> Model -> Html Msg
viewTasksForStudents sharedState model =
    div []
        (Dict.values model.tasksStudents
            |> List.map (\task -> ( task.id, TaskViewer.view sharedState task ))
            |> List.map (\( id, task ) -> Html.map (StudentTaskMsg id) task)
        )