module Pages.SheetEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Sheet exposing (Sheet)
import Api.Request.Courses as CoursesRequests
import Api.Request.Sheet as SheetRequests
import Array
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbState(..)
        , dateInputElement
        , fileUploader
        , inputElement
        , normalPage
        , pageContainer
        , r1Column
        , r2Column
        , rContainer
        , rRow
        , rRowButton
        , rRowExtraSpacing
        , simpleDialog
        , sliderInputElement
        , timeInputElement
        )
import Components.Dialog as Dialog
import Components.Toasty
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, preventDefaultOn)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Time
import TimePicker exposing (TimeEvent(..), TimePicker)
import Toasty
import Utils.DateAndTimeUtils as DTU
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors, perform)
import Validate exposing (Validator, ifBlank, ifNothing, validate)


type Msg
    = NavigateTo Route
    | PublishedTimePickerMsg TimePicker.Msg
    | PublishedDatePickerMsg DatePicker.Msg
    | DeadlineTimePickerMsg TimePicker.Msg
    | DeadlineDatePickerMsg DatePicker.Msg
    | GetRealOffset
    | SheetGetResponse (WebData Sheet)
    | Create
    | CreateResponse (WebData Sheet)
    | Update
    | UpdateResponse (WebData ())
    | RequestDelete
    | Delete
    | DeleteResponse (WebData ())
    | SetField Field String
    | GotFiles File (List File)
    | Pick
    | DragEnter
    | DragLeave
    | FileUploadResponse (WebData ())
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | DeleteSheetDialogShown Bool
    | NoOp


type alias Model =
    { course_id : Int
    , id : Int
    , name : String
    , publishedTimePicker : TimePicker
    , publishedDatePicker : DatePicker.DatePicker
    , publishedAtDate : Maybe Date
    , publishedAtTime : Maybe TimePicker.Time
    , publishedPosix : Maybe Time.Posix
    , deadlineTimePicker : TimePicker
    , deadlineDatePicker : DatePicker.DatePicker
    , deadlineAtDate : Maybe Date
    , deadlineAtTime : Maybe TimePicker.Time
    , deadlinePosix : Maybe Time.Posix
    , utcOffsetPos : Int
    , sheetResponse : WebData Sheet
    , createSheet : Bool
    , hover : Bool
    , file : Maybe File
    , fileChanged : Bool
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    , deleteSheetDialogState : Dialog.State
    }


initModel : ( Model, Cmd Msg )
initModel =
    let
        ( publishedDatePicker, publishedDatePickerFx ) =
            DatePicker.init

        ( deadlineDatePicker, deadlineDatePickerFx ) =
            DatePicker.init
    in
    ( { course_id = 0
      , id = 0
      , name = ""
      , publishedTimePicker = TimePicker.init Nothing
      , publishedDatePicker = publishedDatePicker
      , publishedAtDate = Nothing
      , publishedAtTime = Nothing
      , publishedPosix = Nothing
      , deadlineTimePicker = TimePicker.init Nothing
      , deadlineDatePicker = deadlineDatePicker
      , deadlineAtDate = Nothing
      , deadlineAtTime = Nothing
      , deadlinePosix = Nothing
      , utcOffsetPos = DTU.utcZeroOffsetIndex
      , sheetResponse = NotAsked
      , createSheet = True
      , hover = False
      , file = Nothing
      , fileChanged = False
      , errors = []
      , toasties = Toasty.initialState
      , deleteSheetDialogState = False
      }
    , Cmd.batch
        [ Cmd.map PublishedDatePickerMsg publishedDatePickerFx
        , Cmd.map DeadlineDatePickerMsg deadlineDatePickerFx
        , perform GetRealOffset
        ]
    )


initCreate : Int -> ( Model, Cmd Msg )
initCreate courseId =
    let
        ( model, cmd ) =
            initModel
    in
    ( { model | course_id = courseId }, cmd )


initEdit : Int -> Int -> ( Model, Cmd Msg )
initEdit courseId id =
    let
        ( model, cmd ) =
            initModel
    in
    ( { model
        | createSheet = False
        , course_id = courseId
      }
    , Cmd.batch [ cmd, SheetRequests.sheetGet courseId id SheetGetResponse ]
    )


createRequest : Model -> ( Model, Cmd Msg )
createRequest model =
    case ( model.publishedPosix, model.deadlinePosix ) of
        ( Just publish, Just deadline ) ->
            ( model
            , CoursesRequests.courseSheetsPost model.course_id (setupSheet model.name publish deadline) CreateResponse
            )

        ( _, _ ) ->
            ( model, Cmd.none )
                |> addToast (Components.Toasty.Error "Error" "There was an error with the data provided.")


updateRequest : Model -> ( Model, Cmd Msg )
updateRequest model =
    case ( model.publishedPosix, model.deadlinePosix ) of
        ( Just publish, Just deadline ) ->
            ( model
            , SheetRequests.sheetPut model.course_id model.id (setupSheet model.name publish deadline) UpdateResponse
            )

        ( _, _ ) ->
            ( model, Cmd.none )
                |> addToast (Components.Toasty.Error "Error" "There was an error with the data provided.")


fillModelFromRequest : SharedState -> Model -> Sheet -> Model
fillModelFromRequest sharedState model sheet =
    let
        timezone =
            Maybe.withDefault Time.utc sharedState.timezone

        ( publishedDate, publishedTime, _ ) =
            DTU.splitPosixInDateTimeAndOffset timezone sheet.publish_at

        ( deadlineDate, deadlineTime, offset ) =
            DTU.splitPosixInDateTimeAndOffset timezone sheet.due_at

        utcOffsetPos =
            Maybe.withDefault DTU.utcZeroOffsetIndex <| DTU.findIndexFromParts offset
    in
    { model
        | id = sheet.id
        , name = sheet.name
        , publishedTimePicker = TimePicker.init (Just publishedTime)
        , publishedAtTime = Just publishedTime
        , publishedAtDate = Just publishedDate
        , publishedPosix = Just sheet.publish_at
        , deadlineTimePicker = TimePicker.init (Just deadlineTime)
        , deadlineAtTime = Just deadlineTime
        , deadlineAtDate = Just deadlineDate
        , deadlinePosix = Just sheet.due_at
        , utcOffsetPos = utcOffsetPos
    }


setupSheet : String -> Time.Posix -> Time.Posix -> Sheet
setupSheet name publish deadline =
    { id = 0
    , name = name
    , publish_at = publish
    , due_at = deadline
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        PublishedTimePickerMsg subMsg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update timePickerSettings subMsg model.publishedTimePicker

                newTime =
                    case timeEvent of
                        NoChange ->
                            model.publishedAtTime

                        Changed time ->
                            time
            in
            ( { model
                | publishedTimePicker = updatedPicker
                , publishedAtTime = newTime
                , publishedPosix = testIfTimeIsReady model.publishedAtDate newTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        PublishedDatePickerMsg subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (datePickerSettings sharedState) subMsg model.publishedDatePicker

                newDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.publishedAtDate
            in
            ( { model
                | publishedAtDate = newDate
                , publishedDatePicker = newDatePicker
                , publishedPosix = testIfTimeIsReady newDate model.publishedAtTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        DeadlineTimePickerMsg subMsg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update timePickerSettings subMsg model.deadlineTimePicker

                newTime =
                    case timeEvent of
                        NoChange ->
                            model.deadlineAtTime

                        Changed time ->
                            time
            in
            ( { model
                | deadlineTimePicker = updatedPicker
                , deadlineAtTime = newTime
                , deadlinePosix = testIfTimeIsReady model.deadlineAtDate newTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        DeadlineDatePickerMsg subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (datePickerSettings sharedState) subMsg model.deadlineDatePicker

                newDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.deadlineAtDate
            in
            ( { model
                | deadlineAtDate = newDate
                , deadlineDatePicker = newDatePicker
                , deadlinePosix = testIfTimeIsReady newDate model.deadlineAtTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        GetRealOffset ->
            ( { model
                | utcOffsetPos =
                    Maybe.withDefault DTU.utcZeroOffsetIndex <|
                        DTU.timeZoneToIndex <|
                            Maybe.withDefault Time.utc sharedState.timezone
              }
            , Cmd.none
            , NoUpdate
            )

        SheetGetResponse response ->
            updateHandleGetSheet sharedState model response

        Create ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    let
                        ( newModel, newCmd ) =
                            createRequest model
                    in
                    ( { newModel | errors = [] }, newCmd, NoUpdate )

        CreateResponse response ->
            updateHandleSend sharedState model response

        Update ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    let
                        ( newModel, newCmd ) =
                            updateRequest model
                    in
                    ( { newModel | errors = [] }, newCmd, NoUpdate )

        UpdateResponse response ->
            updateHandleSend sharedState model response

        RequestDelete ->
            --Show Dialog
            ( { model | deleteSheetDialogState = True }, Cmd.none, NoUpdate )

        Delete ->
            ( model, SheetRequests.sheetDelete model.course_id model.id DeleteResponse, NoUpdate )

        DeleteResponse response ->
            updateHandleDelete sharedState model response

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        Pick ->
            ( model, Select.files [ "application/zip" ] GotFiles, NoUpdate )

        DragEnter ->
            ( { model | hover = True }, Cmd.none, NoUpdate )

        DragLeave ->
            ( { model | hover = False }, Cmd.none, NoUpdate )

        GotFiles file files ->
            ( { model | hover = False, file = Just file, fileChanged = True }
            , Cmd.none
            , NoUpdate
            )

        FileUploadResponse response ->
            updateHandleFileUpload sharedState model response

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )

        DeleteSheetDialogShown visible ->
            ( { model | deleteSheetDialogState = visible }, Cmd.none, NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


updateHandleDelete : SharedState -> Model -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleDelete sharedState model response =
    case response of
        RemoteData.Success _ ->
            ( model, perform <| NavigateTo <| CourseDetailRoute model.course_id, NoUpdate )

        RemoteData.Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    -- Differentiate between errros
                    let
                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" "Failed to delete")
                    in
                    ( newModel, newCmd, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


updateHandleGetSheet : SharedState -> Model -> WebData Sheet -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleGetSheet sharedState model response =
    case response of
        Success sheet ->
            let
                newModel =
                    fillModelFromRequest sharedState model sheet
            in
            ( { newModel
                | sheetResponse = response
              }
            , Cmd.none
            , NoUpdate
            )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" "Error receiving sheet data")
                    in
                    ( { newModel | sheetResponse = response }, newCmd, NoUpdate )
                )
                err

        _ ->
            ( { model | sheetResponse = response }, Cmd.none, NoUpdate )


updateHandleSend : SharedState -> Model -> WebData ret -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleSend sharedState model response =
    case response of
        Success _ ->
            case ( model.fileChanged, model.file ) of
                ( True, Just file ) ->
                    ( model, SheetRequests.sheetFilePost model.course_id model.id file FileUploadResponse, NoUpdate )

                ( True, Nothing ) ->
                    ( model, Cmd.none, NoUpdate )

                -- File deleted?
                ( False, _ ) ->
                    ( model, pushUrl sharedState.navKey (reverseRoute <| SheetDetailRoute model.course_id model.id), NoUpdate )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        errorString =
                            case e of
                                Http.BadStatus 400 ->
                                    "Bad Data. Something is off in the data."

                                Http.BadStatus 403 ->
                                    "You are not allowed to do this!"

                                Http.BadBody message ->
                                    "Bad return: " ++ message

                                _ ->
                                    "Something other went wrong"

                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" errorString)
                    in
                    ( newModel, newCmd, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


updateHandleFileUpload : SharedState -> Model -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleFileUpload sharedState model response =
    case response of
        Success _ ->
            ( model, pushUrl sharedState.navKey (reverseRoute <| SheetDetailRoute model.course_id model.id), NoUpdate )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        errorString =
                            case e of
                                Http.BadStatus 400 ->
                                    "Bad Data. Something is off in the data."

                                Http.BadStatus 403 ->
                                    "You are not allowed to do this!"

                                Http.BadBody message ->
                                    "Bad return: " ++ message

                                _ ->
                                    "Something other went wrong"

                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" errorString)
                    in
                    ( newModel, newCmd, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


joinTime : Date -> TimePicker.Time -> Int -> Time.Posix
joinTime date time utcOffsetPos =
    let
        offsetPartsArray =
            Array.fromList DTU.utcOffsetsPartsList

        offset =
            Maybe.withDefault { multiplier = 1, hours = 0, minutes = 0 } <|
                Array.get utcOffsetPos offsetPartsArray
    in
    DTU.joinDateTimeAndOffset date time offset


testIfTimeIsReady : Maybe Date -> Maybe TimePicker.Time -> Int -> Maybe Time.Posix
testIfTimeIsReady maybeDate maybeTime offset =
    case ( maybeDate, maybeTime ) of
        ( Just date, Just time ) ->
            Just <| joinTime date time offset

        ( _, _ ) ->
            Nothing


view : SharedState -> Model -> Html Msg
view sharedState model =
    pageContainer
        [ Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
        , viewDeleteSheetDialog sharedState model
        , normalPage
            [ viewFormLoadingOrError sharedState model ]
        ]


viewFormLoadingOrError : SharedState -> Model -> Html Msg
viewFormLoadingOrError sharedState model =
    case model.sheetResponse of
        Loading ->
            -- Display Spinner
            div [] []

        Failure (Http.BadStatus 400) ->
            text "Wrong Format"

        Failure (Http.BadStatus 401) ->
            text "Not Logged In"

        Failure (Http.BadStatus 403) ->
            text "Not permitted"

        _ ->
            -- In all other cases display the form
            viewForm sharedState model


viewForm : SharedState -> Model -> Html Msg
viewForm sharedState model =
    let
        offsetLabelsArray =
            Array.fromList DTU.utcOffsetLabelsList
    in
    rContainer <|
        [ h1
            [ Styles.headerStyle ]
            [ text <|
                if model.createSheet then
                    "Blatt erstellen"

                else
                    "Blatt bearbeiten"
            ]
        , rRow
            [ fileUploader model.hover model.file DragEnter DragLeave Pick GotFiles ]
        , rRowExtraSpacing <|
            r1Column <|
                inputElement
                    { label = "Sheet Name"
                    , placeholder = "Name"
                    , fieldType = "text"
                    , value = model.name
                    }
                    Name
                    model.errors
                    SetField
        , rRow <|
            r2Column
                (dateInputElement
                    { label = "Published date"
                    , value = model.publishedAtDate
                    , datePicker = model.publishedDatePicker
                    , settings = datePickerSettings sharedState
                    }
                    PublishedDate
                    model.errors
                    PublishedDatePickerMsg
                )
                (timeInputElement
                    { label = "Published time"
                    , placeholder = "Select time..."
                    , timePicker = model.publishedTimePicker
                    , settings = timePickerSettings
                    }
                    PublishedTime
                    model.errors
                    PublishedTimePickerMsg
                )
        , rRow <|
            r2Column
                (dateInputElement
                    { label = "Deadline date"
                    , value = model.deadlineAtDate
                    , datePicker = model.deadlineDatePicker
                    , settings = datePickerSettings sharedState
                    }
                    DeadlineDate
                    model.errors
                    DeadlineDatePickerMsg
                )
                (timeInputElement
                    { label = "Deadline time"
                    , placeholder = "Select time..."
                    , timePicker = model.deadlineTimePicker
                    , settings = timePickerSettings
                    }
                    DeadlineTime
                    model.errors
                    DeadlineTimePickerMsg
                )
        , rRow <|
            r1Column <|
                sliderInputElement
                    { label = "UTC Offset"
                    , value = model.utcOffsetPos
                    , min = 0
                    , max = Array.length offsetLabelsArray - 1
                    , step = 1
                    , valueLabel = Maybe.withDefault "Z" <| Array.get model.utcOffsetPos offsetLabelsArray
                    }
                    UtcOffset
                    model.errors
                    SetField
        , rRowButton <|
            PbbButton <|
                PbbActive
                    (if model.createSheet then
                        "Erstellen"

                     else
                        "Bearbeiten"
                    )
                    (if model.createSheet then
                        Create

                     else
                        Update
                    )
        ]
            ++ (if not model.createSheet then
                    [ rRowButton <|
                        PbbButton <|
                            PbbActive
                                "Löschen"
                                RequestDelete
                    ]

                else
                    []
               )


viewDeleteSheetDialog : SharedState -> Model -> Html Msg
viewDeleteSheetDialog sharedState model =
    simpleDialog
        "Delete the sheet?"
        "Are you sure you want to delete the sheet? This cannot be undone. The sheet and everything associated with the sheet like submissions and tests are gone."
        [ ( "Delete", Styles.buttonRedStyle, Delete )
        , ( "Cancel", Styles.buttonGreenStyle, DeleteSheetDialogShown False )
        ]
        model.deleteSheetDialogState
        deleteSheetDialogConfig


timePickerSettings : TimePicker.Settings
timePickerSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
    { defaultSettings | showSeconds = False, minuteStep = 15, use24Hours = True }


datePickerSettings : SharedState -> DatePicker.Settings
datePickerSettings sharedState =
    let
        curTime =
            Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime
    in
    { defaultSettings
        | inputAttributes =
            [ Styles.lineInputStyle
            , classes [ TC.w_100, TC.mb3 ]
            ]

        -- , dateFormatter = DF.dateToShortFormatString sharedState
        , dayFormatter = DF.shortDayFormatter sharedState
        , monthFormatter = DF.monthFormatter sharedState
    }


type alias Error =
    ( Field, String )


type Field
    = Name
    | PublishedTime
    | PublishedDate
    | DeadlineTime
    | DeadlineDate
    | UtcOffset


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Name ->
            { model | name = value }

        UtcOffset ->
            let
                newPos =
                    Maybe.withDefault DTU.utcZeroOffsetIndex <|
                        String.toInt value
            in
            { model
                | utcOffsetPos = newPos
                , deadlinePosix = testIfTimeIsReady model.deadlineAtDate model.deadlineAtTime newPos
                , publishedPosix = testIfTimeIsReady model.publishedAtDate model.publishedAtTime newPos
            }

        _ ->
            -- times are set by TimePicker
            model


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .name ( Name, "Bitte gib einen Blattnamen ein." )
        , Validate.firstError
            [ ifNothing .publishedAtTime ( PublishedTime, "Bitte gib eine Startzeit ein." )
            , ifFirstPosixNotOlder .publishedPosix .deadlinePosix ( PublishedTime, "Die Startzeit muss vor der Endzeit liegen." )
            ]
        , Validate.firstError
            [ ifNothing .publishedAtDate ( PublishedDate, "Bitte gib ein Startdatum ein." )
            , ifFirstPosixNotOlder .publishedPosix .deadlinePosix ( PublishedDate, "Die Startzeit muss vor der Endzeit liegen." )
            ]
        , Validate.firstError
            [ ifNothing .deadlineAtTime ( DeadlineTime, "Bitte gib eine Endzeit ein." )
            , ifFirstPosixNotOlder .publishedPosix .deadlinePosix ( DeadlineTime, "Die Startzeit muss vor der Endzeit liegen." )
            ]
        , Validate.firstError
            [ ifNothing .deadlineAtDate ( DeadlineDate, "Bitte gib ein Enddatum ein." )
            , ifFirstPosixNotOlder .publishedPosix .deadlinePosix ( DeadlineDate, "Die Startzeit muss vor der Endzeit liegen." )
            ]
        ]


ifFirstPosixNotOlder : (subject -> Maybe Time.Posix) -> (subject -> Maybe Time.Posix) -> error -> Validator error subject
ifFirstPosixNotOlder subjectToMaybePosix1 subjectToMaybePosix2 error =
    Validate.ifFalse (\subject -> isFirstDateOlder (subjectToMaybePosix1 subject) (subjectToMaybePosix2 subject)) error


isFirstDateOlder : Maybe Time.Posix -> Maybe Time.Posix -> Bool
isFirstDateOlder maybeFirstPosix maybeSecondPosix =
    case ( maybeFirstPosix, maybeSecondPosix ) of
        ( Just firstPosix, Just secondPosix ) ->
            Time.posixToMillis firstPosix < Time.posixToMillis secondPosix

        ( _, _ ) ->
            -- Don't show when some fields are not set. Add a manual ifNothing with firstError
            True


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )


deleteSheetDialogConfig : Dialog.Config Msg
deleteSheetDialogConfig =
    Dialog.Config
        Styles.dialogVisibleStyle
        Styles.dialogGoneStyle
        DeleteSheetDialogShown
        True
        NoOp
