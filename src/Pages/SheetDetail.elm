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
import Api.Endpoint exposing (unwrap, sheetFile)
import Browser.Navigation exposing (pushUrl)
import Components.Tasks.AdminView as TaskAdminView
import Components.Tasks.StudentView as TaskStudentView
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
import File.Download as Download
import Utils.DateFormatter as DF
import Components.CommonElements exposing 
    ( pageContainer
    , normalPage
    , rContainer
    , rRow
    , rRowExtraSpacing
    , rRowHeader
    , rRowHeaderActionButtons
    , rRowWarning
    , datesDisplayContainer
    , dateElement
    )


type Msg
    = NavigateTo Route
    | TaskMsg Int TaskMsgTypes
    | UploadProgressMsg Http.Progress
    | GetTaskFetchResponse (WebData (List Task))
    | GetEnrollmentResponse (WebData (List AccountEnrollment))
    | GetSheetDetailResponse (WebData Sheet)
    | DownloadSheet Int Int


type TaskMsgTypes
    = AdminTaskMsg TaskAdminView.Msg
    | StudentTaskMsg TaskStudentView.Msg


type TaskModel
    = AdminTaskModel TaskAdminView.Model
    | StudentTaskModel TaskStudentView.Model


type alias Model =
    { id : Int
    , course_id : Int
    , role : Maybe CourseRole
    , taskResponse : WebData (List Task)
    , taskDict : Dict Int TaskModel
    , sheetDetailResponse : WebData Sheet
    }


init : Int -> Int -> ( Model, Cmd Msg )
init course_id id =
    ( { id = id
      , course_id = course_id
      , role = Nothing
      , taskDict = Dict.empty
      , taskResponse = Loading
      , sheetDetailResponse = Loading
      }
    , Cmd.batch
        -- Query the role and all tasks
        [ SheetRequests.sheetTasksGet course_id id GetTaskFetchResponse
        , SheetRequests.sheetGet course_id id GetSheetDetailResponse
        , AccountRequests.accountEnrollmentGet GetEnrollmentResponse
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        UploadProgressMsg progress ->
            List.foldl
                (\( taskId, modelType ) ( updatedModel, cmd, _ ) ->
                    updateTask sharedState
                        updatedModel
                        taskId
                        cmd
                        (case modelType of
                            AdminTaskModel _ ->
                                AdminTaskMsg <|
                                    TaskAdminView.UploadProgress progress

                            StudentTaskModel _ ->
                                StudentTaskMsg <|
                                    TaskStudentView.UploadProgress progress
                        )
                )
                ( model, Cmd.none, NoUpdate )
                (Dict.toList model.taskDict)

        GetTaskFetchResponse response ->
            let
                ( newModel, cmds ) =
                    fillModelTaskDict { model | taskResponse = response }
            in
            ( newModel, cmds, NoUpdate )

        GetSheetDetailResponse response ->
            ( { model | sheetDetailResponse = response }, Cmd.none, NoUpdate )

        DownloadSheet courseId sheetId ->
            ( model, Download.url <| unwrap <| sheetFile courseId sheetId, NoUpdate)

        GetEnrollmentResponse (Success roles) ->
            let
                maybeRole =
                    roles
                        |> List.filter (\enrollment -> model.course_id == enrollment.course_id)
                        |> List.map (\enrollment -> enrollment.role)
                        |> List.head
            in
            case maybeRole of
                Just role ->
                    let
                        ( newModel, cmds ) =
                            fillModelTaskDict { model | role = maybeRole }
                    in
                    ( newModel, cmds, NoUpdate )

                Nothing ->
                    ( model, Utils.perform <| NavigateTo CoursesRoute, NoUpdate )

        GetEnrollmentResponse response ->
            ( model, Cmd.none, NoUpdate )

        TaskMsg id taskMsg ->
            updateTask sharedState model id Cmd.none taskMsg


updateTask : SharedState -> Model -> Int -> Cmd Msg -> TaskMsgTypes -> ( Model, Cmd Msg, SharedStateUpdate )
updateTask sharedState model id currentCmds taskMsg =
    case getUpdateForTasks sharedState model id taskMsg of
        Just ( newModel, newCmd, newSharedState ) ->
            ( { model
                | taskDict =
                    Dict.update id
                        (Maybe.map (\_ -> newModel))
                        model.taskDict
              }
            , Cmd.batch [ currentCmds, Cmd.map (TaskMsg id) newCmd ]
            , newSharedState
            )

        Nothing ->
            ( model, Cmd.none, NoUpdate )


getUpdateForTasks : SharedState -> Model -> Int -> TaskMsgTypes -> Maybe ( TaskModel, Cmd TaskMsgTypes, SharedStateUpdate )
getUpdateForTasks sharedState model id taskMsg =
    case ( taskMsg, Dict.get id model.taskDict ) of
        ( AdminTaskMsg subMsg, Just (AdminTaskModel taskModel) ) ->
            let
                ( newModel, newCmd, newSharedState ) =
                    TaskAdminView.update sharedState subMsg taskModel
            in
            Just ( AdminTaskModel newModel, Cmd.map AdminTaskMsg newCmd, newSharedState )

        ( StudentTaskMsg subMsg, Just (StudentTaskModel taskModel) ) ->
            let
                ( newModel, newCmd, newSharedState ) =
                    TaskStudentView.update sharedState subMsg taskModel
            in
            Just ( StudentTaskModel newModel, Cmd.map StudentTaskMsg newCmd, newSharedState )

        ( _, _ ) ->
            Nothing


fillModelTaskDict : Model -> ( Model, Cmd Msg )
fillModelTaskDict model =
    case ( model.role, model.taskResponse ) of
        ( Just role, Success tasks ) ->
            case role of
                Admin ->
                    let
                        taskIdModelCmdsList =
                            tasks
                                |> List.map
                                    (\task ->
                                        ( task.id
                                        , TaskAdminView.initFromTask model.course_id task
                                        )
                                    )
                                |> Utils.flip List.append 
                                    [ 
                                        ( -1, TaskAdminView.initCreate model.course_id model.id)
                                    ]
                    in
                    ( { model
                        | taskDict =
                            taskIdModelCmdsList
                                |> List.map
                                    (\( id, ( taskModel, _ ) ) ->
                                        ( id, AdminTaskModel taskModel )
                                    )
                                |> Dict.fromList
                      }
                    , Cmd.batch
                        (taskIdModelCmdsList
                            |> List.map
                                (\( id, ( _, cmd ) ) ->
                                    Cmd.map (TaskMsg id) <| Cmd.map AdminTaskMsg cmd
                                )
                        )
                    )

                Student ->
                    let
                        taskIdModelCmdsList =
                            tasks
                                |> List.map (\task -> ( task.id, TaskStudentView.init model.course_id task ))
                    in
                    ( { model
                        | taskDict =
                            taskIdModelCmdsList
                                |> List.map
                                    (\( id, ( taskModel, _ ) ) ->
                                        ( id, StudentTaskModel taskModel )
                                    )
                                |> Dict.fromList
                      }
                    , Cmd.batch
                        (taskIdModelCmdsList
                            |> List.map
                                (\( id, ( _, cmd ) ) ->
                                    Cmd.map (TaskMsg id) <| Cmd.map StudentTaskMsg cmd
                                )
                        )
                    )

                Tutor ->
                    ( model, Cmd.none )

        --TODO create tutor view
        ( _, _ ) ->
            ( model, Cmd.none )


view : SharedState -> Model -> Html Msg
view sharedState model =
    pageContainer
        [ normalPage <|
            [ viewSheetDetail sharedState model
            , rRowHeader "Tasks"
            , viewTasks sharedState model
            ]
        ]

viewSheetDetail : SharedState -> Model -> Html Msg
viewSheetDetail sharedState model =
    case model.sheetDetailResponse of
        Success detail ->
            rContainer <|
                [ rRowHeaderActionButtons detail.name Styles.headerStyle
                    ([ ("Download", DownloadSheet model.course_id model.id, Styles.buttonGreenStyle) 
                    ] ++ 
                        if model.role == Just Admin then
                            [ ("Edit", NavigateTo <| EditSheetRoute model.course_id model.id, Styles.buttonGreyStyle) ]
                        else
                            []
                    )
                , if checkIfSheetStillActive sharedState detail.due_at then
                    rRowWarning "Submission closed" <| 
                        "The sheet was due " ++ 
                        DF.shortDateFormatter sharedState detail.due_at ++
                        " at " ++ DF.timeFormatter sharedState detail.due_at
                else
                    text ""
                , datesDisplayContainer <|
                    (dateElement "Abgabezeit" <| DF.dateAndTimeFormatter sharedState detail.due_at)

                ]


        Failure err ->
            text "Error loading"

        _ ->
            text "Loading"


viewTasks : SharedState -> Model -> Html Msg
viewTasks sharedState model =
    case model.sheetDetailResponse of
        Success detail ->
            div [ classes [ TC.mh3, TC.pa1 ] ]
                (Dict.values model.taskDict
                    |> List.map
                        (\taskModelType ->
                            case taskModelType of
                                AdminTaskModel taskModel ->
                                    ( taskModel.id
                                    , Html.map AdminTaskMsg <|
                                        TaskAdminView.view sharedState taskModel
                                    )

                                StudentTaskModel taskModel ->
                                    ( taskModel.id
                                    , Html.map StudentTaskMsg <|
                                        TaskStudentView.view sharedState taskModel <|
                                            checkIfSheetStillActive sharedState detail.due_at
                                    )
                        )
                    |> List.map (\( id, taskModel ) -> Html.map (TaskMsg id) taskModel)
                )
        _ ->
            text ""

checkIfSheetStillActive : SharedState -> Time.Posix -> Bool
checkIfSheetStillActive sharedState deadlineTime =
    (Time.posixToMillis deadlineTime) < 
        (Maybe.withDefault 0 <| Maybe.map Time.posixToMillis sharedState.currentTime)