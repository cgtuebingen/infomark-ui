{-
   Grade submission (for tutors)
-}


module Pages.SubmissionGradingEditor exposing (Model, Msg(..), init, update, view)

import Api.Data.Course exposing (Course)
import Api.Data.Grade exposing (ExecutionState(..), Grade, TestStatus(..))
import Api.Data.Group exposing (Group)
import Api.Data.Task exposing (Task)
import Api.Data.User exposing (User)
import Api.Request.Courses as CourseRequests
import Api.Request.Task as TaskRequests
import Api.Request.User as UserRequests
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements as CE
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
import Set exposing (Set)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Spinner
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Utils.Styles as Styles


type Msg
    = NavigateTo Route
    | GetGrades (WebData (List Grade))
    | GetGroups (WebData (List Group))
    | GetTask (WebData Task)
    | GetUserInfo Int (WebData User)
    | GetSendGradeResponse Int (WebData ())
    | ToggleGroupChanger
    | SetField Field String
    | SendGrade Int
    | DownloadSubmission String
    | SpinnerMsg Spinner.Msg


type alias Model =
    { getGradesResponse : WebData (List Grade)
    , getGroupsResponse : WebData (List Group)
    , getTaskResponse : WebData Task
    , getUserResponses : Dict Int (WebData User)
    , getGradeSendResponse : Dict Int (WebData ())
    , fusedGradeDict : Dict Int FusedGrade
    , gradeDoneTime : Dict Int Time.Posix
    , courseId : Int
    , groupId : Int
    , taskId : Int
    , groupChangerVisible : Bool
    , spinner : Spinner.Model
    }


type alias FusedGrade =
    { grade : Grade
    , user : User
    }


type Field
    = Feedback Int
    | Points Int


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init courseId taskId groupId =
    ( { getGradesResponse = Loading
      , getGroupsResponse = Loading
      , getTaskResponse = Loading
      , getUserResponses = Dict.empty
      , getGradeSendResponse = Dict.empty
      , fusedGradeDict = Dict.empty
      , gradeDoneTime = Dict.empty
      , courseId = courseId
      , groupId = groupId
      , taskId = taskId
      , groupChangerVisible = True
      , spinner = Spinner.init
      }
    , Cmd.batch
        [ CourseRequests.courseGradesGetPerTaskAndGroup courseId taskId groupId GetGrades
        , TaskRequests.taskGet courseId taskId GetTask
        , CourseRequests.courseGroupsGet courseId GetGroups
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        GetGrades response ->
            ( fillFusedGradeDict
                { model
                    | getGradesResponse = response
                }
            , case response of
                Success grades ->
                    getAllUsers grades

                _ ->
                    Cmd.none
            , NoUpdate
            )

        GetGroups response ->
            ( { model | getGroupsResponse = response }, Cmd.none, NoUpdate )

        GetTask response ->
            ( { model | getTaskResponse = response }, Cmd.none, NoUpdate )

        GetUserInfo gradeId response ->
            ( fillFusedGradeDict
                { model
                    | getUserResponses = Dict.insert gradeId response model.getUserResponses
                }
            , Cmd.none
            , NoUpdate
            )

        GetSendGradeResponse gradeId response ->
            let
                timeUpdate =
                    { model
                        | gradeDoneTime =
                            Dict.insert gradeId
                                (Maybe.withDefault (Time.millisToPosix 0)
                                    sharedState.currentTime
                                )
                                model.gradeDoneTime
                    }

                timeModel =
                    case response of
                        Success _ ->
                            timeUpdate

                        Failure _ ->
                            timeUpdate

                        _ ->
                            model

                responseModel =
                    { timeModel
                        | getGradeSendResponse = Dict.insert gradeId response model.getGradeSendResponse
                    }
            in
            ( responseModel, Cmd.none, NoUpdate )

        ToggleGroupChanger ->
            ( { model | groupChangerVisible = not model.groupChangerVisible }
            , Cmd.none
            , NoUpdate
            )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        SendGrade gradeId ->
            case Dict.get gradeId model.fusedGradeDict of
                Just fusedGrade ->
                    ( model
                    , TaskRequests.taskGradePut
                        model.courseId
                        gradeId
                        fusedGrade.grade
                        (GetSendGradeResponse gradeId)
                    , NoUpdate
                    )

                Nothing ->
                    ( model, Cmd.none, NoUpdate )

        DownloadSubmission url ->
            ( model, Download.url url, NoUpdate )

        SpinnerMsg subMsg ->
            let
                spinnerModel =
                    Spinner.update subMsg model.spinner
            in
            ( { model | spinner = spinnerModel }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    CE.pageContainer <|
        [ CE.normalPage <|
            case ( model.getGroupsResponse, model.getTaskResponse ) of
                ( Success groups, Success task ) ->
                    let
                        ownGroupData =
                            groups
                                |> List.filter
                                    (\g ->
                                        g.id == model.groupId
                                    )
                                |> List.map
                                    (\g ->
                                        ( g.id
                                        , g.tutor.firstname ++ " " ++ g.tutor.lastname
                                        )
                                    )
                                |> List.head
                                |> Maybe.withDefault ( -1, "" )
                    in
                    [ CE.rContainer <|
                        [ CE.rRowHeader <| "Abgaben für " ++ task.name
                        , CE.rRow <|
                            [ div []
                                [ h4 [ classes [ TC.black, TC.fw6, TC.f5, TC.ttu, TC.lh_copy, TC.tracked, TC.mt3, TC.mb1 ] ]
                                    [ text "Maximale Punkte" ]
                                , h1 [ classes [ TC.dark_red, TC.mt0 ], Styles.headerStyle ]
                                    [ text <|
                                        (String.fromInt <| task.max_points)
                                    ]
                                ]
                            ]
                        , CE.rRowHeader "Abgaben"
                        ]
                            ++ CE.rCollapsable
                                ("Gruppe - " ++ Tuple.second ownGroupData)
                                model.groupChangerVisible
                                ToggleGroupChanger
                                ( "Change Group", "Hide" )
                                [ CE.multiButton <|
                                    (groups
                                        |> List.map
                                            (\g ->
                                                ( g.tutor.firstname ++ " " ++ g.tutor.lastname
                                                , g.id == Tuple.first ownGroupData
                                                , NavigateTo <|
                                                    SubmissionGradingRoute model.courseId model.taskId g.id
                                                )
                                            )
                                    )
                                ]
                            ++ (model.fusedGradeDict
                                    |> Dict.values
                                    |> List.map (viewTask sharedState model task)
                               )
                    ]

                ( _, _ ) ->
                    [ text "" ]
        ]


viewTask : SharedState -> Model -> Task -> FusedGrade -> Html Msg
viewTask sharedState model task fusedGrade =
    let
        noSubmissionElement =
            span [ Styles.headerStyle, classes [ TC.red ] ] [ text "No Submission" ]
    in
    CE.rContainer <|
        [ CE.rRow <|
            CE.r2Column
                [ UserView.view
                    sharedState
                    (Tuple.first <| UserView.initFromUser fusedGrade.user)
                    Nothing
                ]
                [ div
                    [ classes
                        [ TC.w_100
                        , TC.h3_ns
                        , TC.h2
                        , TC.flex
                        , TC.items_center
                        , TC.justify_end
                        , TC.mv3
                        ]
                    ]
                    [ case fusedGrade.grade.file_url of
                        Just url ->
                            if String.isEmpty url then
                                noSubmissionElement

                            else
                                button
                                    [ Styles.buttonGreyStyle
                                    , Styles.pillStyle
                                    , onClick <| DownloadSubmission url
                                    ]
                                    [ text "Download" ]

                        _ ->
                            noSubmissionElement
                    ]
                ]
        ]
            ++ (case fusedGrade.grade.file_url of
                    Just url ->
                        if String.isEmpty url then
                            [ text "" ]

                        else
                            [ CE.rRowExtraSpacing <|
                                CE.r1Column <|
                                    [ CE.inputLabel "Public Test Results"
                                    , CE.renderInTextBox
                                        (case fusedGrade.grade.public_execution_state of
                                            Pending ->
                                                "Pending Test"

                                            Running ->
                                                "Running Test"

                                            Finished ->
                                                fusedGrade.grade.public_test_log
                                        )
                                        True
                                    ]
                            , CE.rRowExtraSpacing <|
                                CE.r1Column <|
                                    [ CE.inputLabel "Private Test Results"
                                    , CE.renderInTextBox
                                        (case fusedGrade.grade.private_execution_state of
                                            Pending ->
                                                "Pending Test"

                                            Running ->
                                                "Running Test"

                                            Finished ->
                                                fusedGrade.grade.private_test_log
                                        )
                                        True
                                    ]
                            ]

                    Nothing ->
                        [ text "" ]
               )
            ++ [ CE.rRowExtraSpacing <|
                    CE.r1Column <|
                        CE.textAreaElement
                            { label = "Feedback"
                            , placeholder = "Write some nice feedback for your student."
                            , value = fusedGrade.grade.feedback
                            }
                            (Feedback fusedGrade.grade.id)
                            []
                            -- TODO do not ignore errors
                            SetField
               , CE.rRowExtraSpacing <|
                    CE.sliderInputElement
                        { label = "Punkte"
                        , value = fusedGrade.grade.acquired_points
                        , min = 0
                        , max = task.max_points
                        , step = 1
                        , valueLabel = String.fromInt fusedGrade.grade.acquired_points ++ " Punkte"
                        }
                        (Points fusedGrade.grade.id)
                        []
                        -- TODO do not ignore errors
                        SetField
               , CE.rRowButton <|
                    let
                        stateShownLongEnough =
                            Maybe.map2
                                (\up cur ->
                                    Time.posixToMillis cur - Time.posixToMillis up > 1500
                                )
                                (Dict.get fusedGrade.grade.id model.gradeDoneTime)
                                sharedState.currentTime
                    in
                    case ( Dict.get fusedGrade.grade.id model.getGradeSendResponse, stateShownLongEnough ) of
                        ( Just (Success _), Just False ) ->
                            CE.PbbButton <| CE.PbbResult <| CE.PbbSuccess "Erfolgreich benotet"

                        ( Just (Failure _), Just False ) ->
                            CE.PbbButton <| CE.PbbResult <| CE.PbbFailure "Fehler beim Benoten"

                        ( Just Loading, _ ) ->
                            CE.PbbSpinner model.spinner

                        ( _, _ ) ->
                            let
                                testDoneButtonState =
                                    if String.isEmpty fusedGrade.grade.feedback then
                                        CE.PbbDisabled "You need to write Feedback"

                                    else
                                        CE.PbbActive "Benoten" (SendGrade fusedGrade.grade.id)

                                submissionAvailable =
                                    (fusedGrade.grade.file_url /= Just "")
                                        && (fusedGrade.grade.file_url /= Nothing)

                                testFinished =
                                    fusedGrade.grade.private_execution_state
                                        == Finished
                                        && fusedGrade.grade.public_execution_state
                                        == Finished
                            in
                            CE.PbbButton <|
                                if not submissionAvailable then
                                    testDoneButtonState

                                else if not testFinished && submissionAvailable then
                                    CE.PbbDisabled "The test results are not completed!"

                                else
                                    testDoneButtonState
               ]


getAllUsers : List Grade -> Cmd Msg
getAllUsers grades =
    grades
        |> List.map (\g -> g.user_id)
        |> List.map (\uid -> UserRequests.userGet uid <| GetUserInfo uid)
        |> Cmd.batch


fillFusedGradeDict : Model -> Model
fillFusedGradeDict model =
    case model.getGradesResponse of
        Success grades ->
            grades
                |> List.map (\g -> g.user_id)
                -- Get all ids
                |> Set.fromList
                |> Set.diff (Set.fromList <| Dict.keys model.getUserResponses)
                -- Check if all ids are in the user dict
                |> (\s ->
                        if Set.isEmpty s then
                            -- All data is available
                            grades
                                |> List.map
                                    (\g ->
                                        case Dict.get g.user_id model.getUserResponses of
                                            Just (Success user) ->
                                                -- User request is gone through too
                                                -- Prepare dict entry
                                                Just ( g.id, { grade = g, user = user } )

                                            _ ->
                                                -- Not done. Return nothing
                                                Nothing
                                    )
                                |> List.filterMap identity
                                -- Filter all nothings..
                                |> Dict.fromList
                                -- ...and create the dict...
                                |> (\d -> { model | fusedGradeDict = d })
                            -- ...which is written to the model

                        else
                            -- Data is not completely loaded
                            model
                   )

        _ ->
            -- Nothing is loaded. Return the old state
            model


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Feedback id ->
            { model
                | fusedGradeDict =
                    Dict.update id
                        (\maybeFusedGrade ->
                            case maybeFusedGrade of
                                Just fusedGrade ->
                                    let
                                        grade =
                                            fusedGrade.grade

                                        gradeUpdate =
                                            { grade
                                                | feedback = value
                                            }
                                    in
                                    Just
                                        { fusedGrade
                                            | grade = gradeUpdate
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.fusedGradeDict
            }

        Points id ->
            { model
                | fusedGradeDict =
                    Dict.update id
                        (\maybeFusedGrade ->
                            case maybeFusedGrade of
                                Just fusedGrade ->
                                    let
                                        grade =
                                            fusedGrade.grade

                                        gradeUpdate =
                                            { grade
                                                | acquired_points =
                                                    Maybe.withDefault 0 <|
                                                        String.toInt value
                                            }
                                    in
                                    Just
                                        { fusedGrade
                                            | grade = gradeUpdate
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.fusedGradeDict
            }
