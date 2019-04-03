module Components.Tasks.StudentView exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    , subscriptions
    )

{-| The non admin counter part of TaskEditor.
Can be used to upload submissions and view the
public test results.
-}

import Api.Data.Grade exposing (Grade, ExecutionState)
import Api.Data.Task exposing (Task)
import Api.Data.TaskRatingResponse exposing (TaskRatingResponse)
import Api.Request.Task as TaskRequests
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbResultState(..)
        , PbbState(..)
        , fileUploader
        , inputLabel
        , r1Column
        , r2Column
        , rCollapsable
        , rContainer
        , rRow
        , rRowButton
        , rRowExtraSpacing
        , renderInTextBox
        , sliderInputElement
        )
import Debounce exposing (Debounce)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Markdown as MD
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Time
import Task
import Utils.Styles as Styles
import Utils.Utils exposing (perform)


type Field
    = Rating


type Msg
    = UploadSubmission
    | UploadSubmissionResponse (WebData ())
    | UploadProgress Http.Progress
    | GetCurrentRatingResponse (WebData TaskRatingResponse)
    | GetGradeResponse (WebData Grade) -- Change return type
    | RateTask Field String
    | SendRating Int
    | RateResponse (WebData ())
    | ToggleCollapse
    | GotFiles File (List File)
    | Pick
    | DragEnter
    | DragLeave
    | NoOp
    | DebounceMsg Debounce.Msg
    | Tick Time.Posix


type alias Model =
    { id : Int
    , courseId : Int
    , task : Task
    , gradeResponse : WebData Grade
    , rating : Int
    , submission : Maybe File
    , uploading : WebData ()
    , uploadPercentage : Int
    , uploadDoneTime : Maybe Time.Posix
    , collapse : Bool
    , hover : Bool
    , ratingDebounce : Debounce Int
    , lastPublicTestStateCheck : Maybe Time.Posix
    , pollForPublicTestLogTimeSubscription : Bool
    }


init : Int -> Task -> ( Model, Cmd Msg )
init courseId task =
    ( { id = task.id
      , courseId = courseId
      , task = task
      , gradeResponse = Loading
      , rating = 0
      , submission = Nothing
      , uploading = NotAsked
      , uploadPercentage = 0
      , uploadDoneTime = Nothing
      , collapse = True
      , hover = False
      , ratingDebounce = Debounce.init
      , lastPublicTestStateCheck = Nothing
      --, lastPublicTestStateCheck = Task.perform NoUpdate Time.now
      , pollForPublicTestLogTimeSubscription = False
      }
    , Cmd.batch
        [ TaskRequests.taskResultGet courseId task.id GetGradeResponse
        , TaskRequests.taskRatingGet courseId task.id GetCurrentRatingResponse
        ]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 2000
    , transform = DebounceMsg
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UploadSubmission ->
            case model.submission of
                Just file ->
                    ( { model | uploading = Loading }, TaskRequests.taskSubmissionPost model.courseId model.id file UploadSubmissionResponse, NoUpdate )

                Nothing ->
                    -- Should never happen. Upload button disabled without a set file
                    ( model, Cmd.none, NoUpdate )

        UploadSubmissionResponse (Success _) ->
            ( { model
                | uploadDoneTime = sharedState.currentTime
                , uploading = Success ()
                , lastPublicTestStateCheck = sharedState.currentTime
                , pollForPublicTestLogTimeSubscription = True
              }
            , Cmd.none
            , NoUpdate
            )

        UploadSubmissionResponse response ->
            let
                newModel =
                    { model | uploading = response  }

                finalModel =
                    if
                        not (RemoteData.isLoading newModel.uploading)
                            && (RemoteData.isSuccess newModel.uploading
                                    || RemoteData.isFailure newModel.uploading
                               )
                    then
                        { newModel | uploadDoneTime = sharedState.currentTime }

                    else
                        newModel
            in
            ( finalModel, Cmd.none, NoUpdate )

        UploadProgress progress ->
            let
                percentage =
                    case progress of
                        Http.Sending p ->
                            Http.fractionSent p

                        _ ->
                            0.0

                prog =
                    if model.uploading == Loading then
                        round <| (100 * percentage)

                    else
                        round <| 0
            in
            ( { model | uploadPercentage = prog }, Cmd.none, NoUpdate )

        GetCurrentRatingResponse (Success rating) ->
            ( { model | rating = rating.own_rating }, Cmd.none, NoUpdate )

        GetCurrentRatingResponse _ ->
            ( model, Cmd.none, NoUpdate )

        GetGradeResponse response ->
            ( { model | gradeResponse = response }, Cmd.none, NoUpdate )

        RateTask _ rating ->
            let
                toInt =
                    Maybe.withDefault 0 <| String.toInt rating

                ( debounce, cmd ) =
                    Debounce.push debounceConfig toInt model.ratingDebounce
            in
            ( { model | rating = toInt, ratingDebounce = debounce }, cmd, NoUpdate )

        SendRating rating ->
            ( model, TaskRequests.taskRatingPost model.courseId model.id rating RateResponse, NoUpdate )

        RateResponse response ->
            ( model, Cmd.none, NoUpdate )

        ToggleCollapse ->
            ( { model | collapse = not model.collapse }, Cmd.none, NoUpdate )

        GotFiles file files ->
            ( { model | hover = False, submission = Just file }, Cmd.none, NoUpdate )

        Pick ->
            ( model, Select.files [ "application/zip" ] GotFiles, NoUpdate )

        DragEnter ->
            ( { model | hover = True }, Cmd.none, NoUpdate )

        DragLeave ->
            ( { model | hover = False }, Cmd.none, NoUpdate )

        DebounceMsg subMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast (\r -> perform <| SendRating r))
                        subMsg
                        model.ratingDebounce
            in
            ( { model | ratingDebounce = debounce }
            , cmd
            , NoUpdate
            )

        NoOp ->
            ( model, Cmd.none, NoUpdate )

        Tick time ->
            let
                dummy = Debug.log("hi")
                timeNow = Time.posixToMillis (Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime)
                timeChecked = Time.posixToMillis (Maybe.withDefault (Time.millisToPosix 0) model.lastPublicTestStateCheck)

                pollAgain = (case model.gradeResponse of
                    Success grade ->
                        (grade.public_execution_state /= Api.Data.Grade.Finished)

                    Loading ->
                        False

                    _ ->
                        False
                    )
            in

            if (model.pollForPublicTestLogTimeSubscription == True) && (timeNow - timeChecked > 2000) then
                ( {model | pollForPublicTestLogTimeSubscription = pollAgain}
                , TaskRequests.taskResultGet model.courseId model.task.id GetGradeResponse
                , NoUpdate
                )
            else
                (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Bool -> Html Msg
view sharedState model deadlineReached =
    rContainer <|
        rCollapsable model.task.name
            model.collapse
            ToggleCollapse
            ( "Show", "Hide" )
        <|
            [ rRow <|
                r1Column <|
                    [ inputLabel "Submission"
                    , fileUploader model.hover model.submission DragEnter DragLeave Pick GotFiles
                    ]
            , rRow <|
                r1Column <|
                    [ inputLabel "Test Results"
                    , renderInTextBox
                        (case model.gradeResponse of
                            Success grade ->
                                grade.public_test_log

                            Loading ->
                                "Loading"

                            _ ->
                                "Undefined"
                        )
                        True
                    ]
            ]
                ++ (case model.gradeResponse of
                        Success grade ->
                            if String.isEmpty grade.feedback then
                                [ text "" ]

                            else
                                [ rRow <|
                                    r1Column <|
                                        [ inputLabel "Feedback"
                                        , renderInTextBox
                                            grade.feedback
                                            True
                                        ]
                                , h2 [ classes [ TC.pa4, TC.mt4, TC.bt, TC.bb, TC.bw2, TC.dark_red, TC.b__black ] ]
                                    [ text <|
                                        String.fromInt grade.acquired_points
                                            ++ "/"
                                            ++ String.fromInt model.task.max_points
                                            ++ " Points acquired"
                                    ]
                                ]

                        _ ->
                            [ text "" ]
                   )
                ++ [ rRow <|
                        r1Column <|
                            sliderInputElement
                                { label = "Rating"
                                , value = model.rating
                                , min = 0
                                , max = 5
                                , step = 1
                                , valueLabel =
                                    if model.rating == 0 then
                                        "Not rated"

                                    else
                                        String.fromInt model.rating
                                }
                                Rating
                                []
                                RateTask
                   , if model.uploading == Loading then
                        rRowButton <| PbbProgressBar model.uploadPercentage

                     else
                        let
                            success =
                                RemoteData.isSuccess model.uploading

                            failure =
                                RemoteData.isFailure model.uploading

                            filesSelected =
                                case model.submission of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        False

                            stateShownLongEnough =
                                Maybe.map2
                                    (\up cur ->
                                        Time.posixToMillis cur - Time.posixToMillis up > 1500
                                    )
                                    model.uploadDoneTime
                                    sharedState.currentTime
                        in
                        rRowButton <|
                            PbbButton <|
                                if success && stateShownLongEnough == Just False then
                                    PbbResult <| PbbSuccess "Success"

                                else if failure && stateShownLongEnough == Just False then
                                    PbbResult <| PbbFailure "Failure"

                                else if deadlineReached then
                                    PbbDisabled "Submission closed"

                                else if not filesSelected then
                                    PbbDisabled "Upload"

                                else
                                    PbbActive "Upload" UploadSubmission
                   ]


type alias Error =
    ( Field, String )
