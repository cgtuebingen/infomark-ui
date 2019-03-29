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
import Api.Data.Course exposing (Course)
import Api.Data.CourseRole exposing (CourseRole(..))
import Api.Data.Group exposing (Group)
import Api.Data.PointOverview exposing (PointOverview)
import Api.Data.Sheet exposing (Sheet)
import Api.Data.Task exposing (Task)
import Api.Endpoint exposing (sheetFile, unwrap)
import Api.Request.Account as AccountRequests
import Api.Request.Courses as CourseRequests
import Api.Request.Sheet as SheetRequests
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( dateElement
        , datesDisplayContainer
        , normalPage
        , pageContainer
        , r2Column
        , rContainer
        , rRow
        , rRowExtraSpacing
        , rRowHeader
        , rRowHeaderActionButtons
        , rRowWarning
        )
import Components.Tasks.AdminView as TaskAdminView
import Components.Tasks.StudentView as TaskStudentView
import Components.Tasks.TutorView as TaskTutorView
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
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Time
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Utils.Utils as Utils


type Msg
    = NavigateTo Route
    | TaskMsg Int TaskMsgTypes
    | UploadProgressMsg Http.Progress
    | GetTaskFetchResponse (WebData (List Task))
    | GetEnrollmentResponse (WebData (List AccountEnrollment))
    | GetSheetDetailResponse (WebData Sheet)
    | GetPointOverview (WebData (List PointOverview))
    | GetCourseResponse (WebData Course)
    | GetOwnGroupsResponse (WebData (List Group))
    | DownloadSheet Int Int


type TaskMsgTypes
    = AdminTaskMsg TaskAdminView.Msg
    | StudentTaskMsg TaskStudentView.Msg
    | TutorTaskMsg TaskTutorView.Msg


type TaskModel
    = AdminTaskModel TaskAdminView.Model
    | StudentTaskModel TaskStudentView.Model
    | TutorTaskModel TaskTutorView.Model


type alias Model =
    { id : Int
    , course_id : Int
    , requiredPercentage : Maybe Int
    , role : Maybe CourseRole
    , taskResponse : WebData (List Task)
    , ownGroupsResponse : WebData (List Group)
    , taskDict : Dict Int TaskModel
    , sheetDetailResponse : WebData Sheet
    , pointOverviewResponse : WebData (List PointOverview)
    }


init : Int -> Int -> ( Model, Cmd Msg )
init course_id id =
    ( { id = id
      , course_id = course_id
      , role = Nothing
      , taskDict = Dict.empty
      , taskResponse = Loading
      , ownGroupsResponse = Loading
      , sheetDetailResponse = Loading
      , pointOverviewResponse = NotAsked
      , requiredPercentage = Nothing
      }
    , Cmd.batch
        -- Query the role and all tasks
        [ SheetRequests.sheetTasksGet course_id id GetTaskFetchResponse
        , SheetRequests.sheetGet course_id id GetSheetDetailResponse
        , AccountRequests.accountEnrollmentGet GetEnrollmentResponse
        , CourseRequests.courseGet course_id GetCourseResponse
        , CourseRequests.courseOwnGroupGet course_id GetOwnGroupsResponse
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

                            _ ->
                                TutorTaskMsg <|
                                    TaskTutorView.NoOp
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
            ( model, Download.url <| unwrap <| sheetFile courseId sheetId, NoUpdate )

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
                    case role of
                        Student ->
                            ( { newModel | pointOverviewResponse = Loading }
                            , Cmd.batch
                                [ cmds
                                , SheetRequests.sheetPointsGet
                                    model.course_id
                                    model.id
                                    GetPointOverview
                                ]
                            , NoUpdate
                            )

                        _ ->
                            ( newModel, cmds, NoUpdate )

                Nothing ->
                    ( model, Utils.perform <| NavigateTo CoursesRoute, NoUpdate )

        GetCourseResponse (Success course) ->
            ( { model | requiredPercentage = Just <| course.required_percentage }
            , Cmd.none
            , NoUpdate
            )

        GetPointOverview response ->
            ( { model | pointOverviewResponse = response }
            , Cmd.none
            , NoUpdate
            )

        GetEnrollmentResponse response ->
            ( model, Cmd.none, NoUpdate )

        TaskMsg id taskMsg ->
            updateTask sharedState model id Cmd.none taskMsg

        GetOwnGroupsResponse response ->
            ( { model | ownGroupsResponse = response }, Cmd.none, NoUpdate )

        _ ->
            ( model, Cmd.none, NoUpdate )


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

        ( TutorTaskMsg subMsg, Just (TutorTaskModel taskModel) ) ->
            let
                ( newModel, newCmd, newSharedState ) =
                    TaskTutorView.update sharedState subMsg taskModel
            in
            Just ( TutorTaskModel newModel, Cmd.map TutorTaskMsg newCmd, newSharedState )

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
                                    [ ( -1, TaskAdminView.initCreate model.course_id model.id )
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
                    case model.ownGroupsResponse of
                        Success groups ->
                            let
                                taskIdModelCmdsList =
                                    tasks
                                        |> List.map (\task -> ( task.id, TaskTutorView.init model.course_id task groups ))
                            in
                            ( { model
                                | taskDict =
                                    taskIdModelCmdsList
                                        |> List.map
                                            (\( id, ( taskModel, _ ) ) ->
                                                ( id, TutorTaskModel taskModel )
                                            )
                                        |> Dict.fromList
                              }
                            , Cmd.batch
                                (taskIdModelCmdsList
                                    |> List.map
                                        (\( id, ( _, cmd ) ) ->
                                            Cmd.map (TaskMsg id) <| Cmd.map TutorTaskMsg cmd
                                        )
                                )
                            )

                        _ ->
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
    let
        maybePoints =
            case model.pointOverviewResponse of
                Success points ->
                    points
                        |> List.map (\p -> ( p.acquired_points, p.max_points ))
                        |> List.foldl
                            (\pt at ->
                                Tuple.mapBoth
                                    ((+) <| Tuple.first pt)
                                    ((+) <| Tuple.second pt)
                                    at
                            )
                            ( 0, 0 )
                        |> (\pt ->
                                ( Tuple.first pt
                                , Tuple.second pt
                                , case model.requiredPercentage of
                                    Just percentage ->
                                        let
                                            acquiredPerc =
                                                round <|
                                                    (toFloat <| Tuple.first pt)
                                                        / (toFloat <| Tuple.second pt)
                                                        * 100
                                        in
                                        if acquiredPerc < percentage then
                                            TC.red

                                        else if acquiredPerc < (percentage + 5) then
                                            TC.gold

                                        else
                                            TC.dark_green

                                    Nothing ->
                                        TC.dark_red
                                )
                           )
                        |> Just

                _ ->
                    Nothing
    in
    case model.sheetDetailResponse of
        Success detail ->
            rContainer <|
                [ rRowHeaderActionButtons detail.name
                    Styles.headerStyle
                    ([ ( "Download", DownloadSheet model.course_id model.id, Styles.buttonGreenStyle )
                     ]
                        ++ (if model.role == Just Admin then
                                [ ( "Edit", NavigateTo <| EditSheetRoute model.course_id model.id, Styles.buttonGreyStyle ) ]

                            else
                                []
                           )
                    )
                , if checkIfSheetStillActive sharedState detail.due_at then
                    rRowWarning "Submission closed" <|
                        "The sheet was due "
                            ++ DF.shortDateFormatter sharedState detail.due_at
                            ++ " at "
                            ++ DF.timeFormatter sharedState detail.due_at

                  else
                    text ""
                , rRow <|
                    r2Column
                        [ datesDisplayContainer <|
                            (dateElement "Abgabezeit" <| DF.dateAndTimeFormatter sharedState detail.due_at)
                                ++ (dateElement "Maximale Punkte" <| text <| String.fromInt <| sumTasksPoints model)
                        ]
                        [ case maybePoints of
                            Just ( acquired, max, color ) ->
                                div []
                                    [ h4 [ classes [ TC.black, TC.fw6, TC.f5, TC.ttu, TC.lh_copy, TC.tracked, TC.mt3, TC.mb1 ] ]
                                        [ text "Erreichte Punkte" ]
                                    , h1 [ classes [ color, TC.mt0 ], Styles.headerStyle ]
                                        [ text <|
                                            (String.fromInt <| acquired)
                                                ++ "/"
                                                ++ (String.fromInt <| max)
                                        ]
                                    ]

                            _ ->
                                text ""
                        ]
                ]

        Failure err ->
            text "Error loading"

        _ ->
            text "Loading"


sumTasksPoints : Model -> Int
sumTasksPoints model =
    Dict.values model.taskDict
        |> List.map
            (\taskModel ->
                case taskModel of
                    AdminTaskModel admin ->
                        Maybe.withDefault 0 <|
                            String.toInt admin.max_points

                    StudentTaskModel student ->
                        student.task.max_points

                    TutorTaskModel tutor ->
                        tutor.task.max_points
            )
        |> List.foldl (+) 0


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

                                TutorTaskModel taskModel ->
                                    ( taskModel.id
                                    , Html.map TutorTaskMsg <|
                                        TaskTutorView.view sharedState taskModel
                                    )
                        )
                    |> List.map (\( id, taskModel ) -> Html.map (TaskMsg id) taskModel)
                )

        _ ->
            text ""


checkIfSheetStillActive : SharedState -> Time.Posix -> Bool
checkIfSheetStillActive sharedState deadlineTime =
    Time.posixToMillis deadlineTime
        < (Maybe.withDefault 0 <| Maybe.map Time.posixToMillis sharedState.currentTime)
