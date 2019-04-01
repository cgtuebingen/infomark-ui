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
    , getGradeSendResponse : Dict Int (WebData ())
    , gradeDoneTime : Dict Int Time.Posix
    , gradesDict : Dict Int Grade
    , feedbackDict : Dict Int String
    , courseId : Int
    , groupId : Int
    , taskId : Int
    , groupChangerVisible : Bool
    , spinner : Spinner.Model
    }


type Field
    = Feedback Int
    | Points Int


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init courseId taskId groupId =
    ( { getGradesResponse = Loading
      , getGroupsResponse = Loading
      , getTaskResponse = Loading
      , getGradeSendResponse = Dict.empty
      , gradeDoneTime = Dict.empty
      , gradesDict = Dict.empty
      , feedbackDict = Dict.empty
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
            ( fillGradeDict
                { model
                    | getGradesResponse = response
                }
            , Cmd.none
            , NoUpdate
            )

        GetGroups response ->
            ( { model | getGroupsResponse = response }, Cmd.none, NoUpdate )

        GetTask response ->
            ( { model | getTaskResponse = response }, Cmd.none, NoUpdate )

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
            case ( Dict.get gradeId model.gradesDict, Dict.get gradeId model.feedbackDict ) of
                ( Just grade, Just feedback ) ->
                    ( model
                    , TaskRequests.taskGradePut
                        model.courseId
                        gradeId
                        { grade | feedback = feedback }
                        (GetSendGradeResponse gradeId)
                    , NoUpdate
                    )

                ( _, _ ) ->
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
                            ++ (model.gradesDict
                                    |> Dict.values
                                    |> List.sortWith compareGradeEntries
                                    |> List.map
                                        (\g ->
                                            ( g
                                            , Maybe.withDefault "" <|
                                                Dict.get g.id model.feedbackDict
                                            )
                                        )
                                    |> List.map
                                        (\( g, f ) ->
                                            viewTask sharedState model task g f
                                        )
                               )
                    ]

                ( _, _ ) ->
                    [ text "" ]
        ]


viewTask : SharedState -> Model -> Task -> Grade -> String -> Html Msg
viewTask sharedState model task grade feedback =
    let
        noSubmissionElement =
            span [ Styles.headerStyle, classes [ TC.red ] ] [ text "No Submission" ]
    in
    CE.rContainer <|
        [ CE.rRow <|
            CE.r2Column
                [ UserView.view
                    sharedState
                    (Tuple.first <| UserView.initFromUser grade.user)
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
                    [ case grade.file_url of
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
            ++ (case grade.file_url of
                    Just url ->
                        if String.isEmpty url then
                            [ text "" ]

                        else
                            [ CE.rRowExtraSpacing <|
                                CE.r1Column <|
                                    [ CE.inputLabel "Public Test Results"
                                    , CE.renderInTextBox
                                        (case grade.public_execution_state of
                                            Pending ->
                                                "Pending Test"

                                            Running ->
                                                "Running Test"

                                            Finished ->
                                                grade.public_test_log
                                        )
                                        True
                                    ]
                            , CE.rRowExtraSpacing <|
                                CE.r1Column <|
                                    [ CE.inputLabel "Private Test Results"
                                    , CE.renderInTextBox
                                        (case grade.private_execution_state of
                                            Pending ->
                                                "Pending Test"

                                            Running ->
                                                "Running Test"

                                            Finished ->
                                                grade.private_test_log
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
                            , value = feedback
                            }
                            (Feedback grade.id)
                            []
                            -- TODO do not ignore errors
                            SetField
               , CE.rRowExtraSpacing <|
                    CE.sliderInputElement
                        { label = "Punkte"
                        , value = grade.acquired_points
                        , min = 0
                        , max = task.max_points
                        , step = 1
                        , valueLabel = String.fromInt grade.acquired_points ++ " Punkte"
                        }
                        (Points grade.id)
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
                                (Dict.get grade.id model.gradeDoneTime)
                                sharedState.currentTime
                    in
                    case ( Dict.get grade.id model.getGradeSendResponse, stateShownLongEnough ) of
                        ( Just (Success _), Just False ) ->
                            CE.PbbButton <| CE.PbbResult <| CE.PbbSuccess "Erfolgreich benotet"

                        ( Just (Failure _), Just False ) ->
                            CE.PbbButton <| CE.PbbResult <| CE.PbbFailure "Fehler beim Benoten"

                        ( Just Loading, _ ) ->
                            CE.PbbSpinner model.spinner

                        ( _, _ ) ->
                            let
                                testDoneButtonState =
                                    if String.isEmpty feedback then
                                        CE.PbbDisabled "You need to write Feedback"

                                    else
                                        CE.PbbActive "Benoten" (SendGrade grade.id)

                                submissionAvailable =
                                    (grade.file_url /= Just "")
                                        && (grade.file_url /= Nothing)

                                testFinished =
                                    grade.private_execution_state
                                        == Finished
                                        && grade.public_execution_state
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


fillGradeDict : Model -> Model
fillGradeDict model =
    case model.getGradesResponse of
        Success grades ->
            grades
                |> List.map (\g -> ( g.id, g ))
                |> Dict.fromList
                |> Tuple.pair
                    (grades
                        |> List.map (\g -> ( g.id, g.feedback ))
                        |> Dict.fromList
                    )
                |> (\( fd, gd ) -> { model | gradesDict = gd, feedbackDict = fd })

        _ ->
            -- Nothing is loaded. Return the old state
            model


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Feedback id ->
            { model
                | feedbackDict =
                    Dict.update id
                        (Maybe.map (\_ -> value))
                        model.feedbackDict
            }

        Points id ->
            { model
                | gradesDict =
                    Dict.update id
                        (\maybeGrade ->
                            case maybeGrade of
                                Just grade ->
                                    Just
                                        { grade
                                            | acquired_points =
                                                Maybe.withDefault 0 <|
                                                    String.toInt value
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.gradesDict
            }


compareGradeEntries : Grade -> Grade -> Order
compareGradeEntries fgA fgB =
    let
        joinedName =
            \fg -> fg.user.lastname ++ " " ++ fg.user.firstname
    in
    case ( String.isEmpty fgA.feedback, String.isEmpty fgB.feedback ) of
        ( True, False ) ->
            LT

        ( False, True ) ->
            GT

        ( _, _ ) ->
            -- Sort By name
            compare (joinedName fgA) (joinedName fgB)
