module Pages.MaterialEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Material exposing (Material, MaterialType(..))
import Api.Request.Courses as CoursesRequests
import Api.Request.Material as MaterialRequests
import Array
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbState(..)
        , dateInputElement
        , fileUploader
        , inputElement
        , inputLabel
        , multiButton
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
    | LectureTimePickerMsg TimePicker.Msg
    | LectureDatePickerMsg DatePicker.Msg
    | GetRealOffset
    | MaterialGetResponse (WebData Material)
    | Create
    | CreateResponse (WebData Material)
    | Update
    | UpdateResponse (WebData ())
    | RequestDelete
    | Delete
    | DeleteResponse (WebData ())
    | SetField Field String
    | SetMaterialType MaterialType
    | SetRequiredRole Int
    | GotFiles File (List File)
    | Pick
    | DragEnter
    | DragLeave
    | FileUploadResponse (WebData ())
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | DeleteMaterialDialogShown Bool
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
    , lectureTimePicker : TimePicker
    , lectureDatePicker : DatePicker.DatePicker
    , lectureAtDate : Maybe Date
    , lectureAtTime : Maybe TimePicker.Time
    , lecturePosix : Maybe Time.Posix
    , utcOffsetPos : Int
    , materialResponse : WebData Material
    , createMaterial : Bool
    , materialType : MaterialType
    , requiredRole : Int
    , hover : Bool
    , file : Maybe File
    , fileChanged : Bool
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    , deleteMaterialDialogState : Dialog.State
    }


initModel : ( Model, Cmd Msg )
initModel =
    let
        ( publishedDatePicker, publishedDatePickerFx ) =
            DatePicker.init

        ( lectureDatePicker, lectureDatePickerFx ) =
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
      , lectureTimePicker = TimePicker.init Nothing
      , lectureDatePicker = lectureDatePicker
      , lectureAtDate = Nothing
      , lectureAtTime = Nothing
      , lecturePosix = Nothing
      , utcOffsetPos = DTU.utcZeroOffsetIndex
      , materialResponse = NotAsked
      , createMaterial = True
      , materialType = Slide
      , requiredRole = 0
      , hover = False
      , file = Nothing
      , fileChanged = False
      , errors = []
      , toasties = Toasty.initialState
      , deleteMaterialDialogState = False
      }
    , Cmd.batch
        [ Cmd.map PublishedDatePickerMsg publishedDatePickerFx
        , Cmd.map LectureDatePickerMsg lectureDatePickerFx
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
        | createMaterial = False
        , course_id = courseId
      }
    , Cmd.batch [ cmd, MaterialRequests.materialGet courseId id MaterialGetResponse ]
    )


createRequest : Model -> ( Model, Cmd Msg )
createRequest model =
    case ( model.publishedPosix, model.lecturePosix ) of
        ( Just publish, Just lecture ) ->
            ( model
            , CoursesRequests.courseMaterialsPost model.course_id
                (setupMaterial
                    model.name
                    publish
                    lecture
                    model.materialType
                    model.requiredRole
                )
                CreateResponse
            )

        ( _, _ ) ->
            ( model, Cmd.none )
                |> addToast (Components.Toasty.Error "Error" "There was an error with the data provided.")


updateRequest : Model -> ( Model, Cmd Msg )
updateRequest model =
    case ( model.publishedPosix, model.lecturePosix ) of
        ( Just publish, Just lecture ) ->
            ( model
            , MaterialRequests.materialPut model.course_id
                model.id
                (setupMaterial model.name
                    publish
                    lecture
                    model.materialType
                    model.requiredRole
                )
                UpdateResponse
            )

        ( _, _ ) ->
            ( model, Cmd.none )
                |> addToast (Components.Toasty.Error "Error" "There was an error with the data provided.")


fillModelFromRequest : SharedState -> Model -> Material -> Model
fillModelFromRequest sharedState model material =
    let
        timezone =
            Maybe.withDefault Time.utc sharedState.timezone

        ( publishedDate, publishedTime, _ ) =
            DTU.splitPosixInDateTimeAndOffset timezone material.published_at

        ( lectureDate, lectureTime, offset ) =
            DTU.splitPosixInDateTimeAndOffset timezone material.lecture_at

        utcOffsetPos =
            Maybe.withDefault DTU.utcZeroOffsetIndex <| DTU.findIndexFromParts offset
    in
    { model
        | id = material.id
        , name = material.name
        , materialType = material.material_type
        , publishedTimePicker = TimePicker.init (Just publishedTime)
        , publishedAtTime = Just publishedTime
        , publishedAtDate = Just publishedDate
        , publishedPosix = Just material.published_at
        , lectureTimePicker = TimePicker.init (Just lectureTime)
        , lectureAtTime = Just lectureTime
        , lectureAtDate = Just lectureDate
        , lecturePosix = Just material.lecture_at
        , utcOffsetPos = utcOffsetPos
        , requiredRole = material.required_role
    }


setupMaterial : String -> Time.Posix -> Time.Posix -> MaterialType -> Int -> Material
setupMaterial name publish lecture materialType requiredRole =
    { id = 0
    , file_url = Nothing
    , name = name
    , material_type = materialType
    , published_at = publish
    , lecture_at = lecture
    , required_role = requiredRole
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

        LectureTimePickerMsg subMsg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update timePickerSettings subMsg model.lectureTimePicker

                newTime =
                    case timeEvent of
                        NoChange ->
                            model.lectureAtTime

                        Changed time ->
                            time
            in
            ( { model
                | lectureTimePicker = updatedPicker
                , lectureAtTime = newTime
                , lecturePosix = testIfTimeIsReady model.lectureAtDate newTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        LectureDatePickerMsg subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (datePickerSettings sharedState) subMsg model.lectureDatePicker

                newDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.lectureAtDate
            in
            ( { model
                | lectureAtDate = newDate
                , lectureDatePicker = newDatePicker
                , lecturePosix = testIfTimeIsReady newDate model.lectureAtTime model.utcOffsetPos
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

        MaterialGetResponse response ->
            updateHandleGetMaterial sharedState model response

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
            let
                newModel =
                    case response of
                        Success material ->
                            fillModelFromRequest sharedState model material

                        _ ->
                            model
            in
            updateHandleSend sharedState newModel response

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
            ( { model | deleteMaterialDialogState = True }, Cmd.none, NoUpdate )

        Delete ->
            ( model, MaterialRequests.materialDelete model.course_id model.id DeleteResponse, NoUpdate )

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

        SetMaterialType matType ->
            ( { model | materialType = matType }
            , Cmd.none
            , NoUpdate
            )

        SetRequiredRole role ->
            ( { model | requiredRole = role }
            , Cmd.none
            , NoUpdate
            )

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )

        DeleteMaterialDialogShown visible ->
            ( { model | deleteMaterialDialogState = visible }, Cmd.none, NoUpdate )

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


updateHandleGetMaterial : SharedState -> Model -> WebData Material -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleGetMaterial sharedState model response =
    case response of
        Success material ->
            let
                newModel =
                    fillModelFromRequest sharedState model material
            in
            ( { newModel
                | materialResponse = response
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
                                |> addToast (Components.Toasty.Error "Error" "Error receiving material data")
                    in
                    ( { newModel | materialResponse = response }, newCmd, NoUpdate )
                )
                err

        _ ->
            ( { model | materialResponse = response }, Cmd.none, NoUpdate )


updateHandleSend : SharedState -> Model -> WebData ret -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleSend sharedState model response =
    case response of
        Success _ ->
            case ( model.fileChanged, model.file ) of
                ( True, Just file ) ->
                    ( model, MaterialRequests.materialFilePost model.course_id model.id file FileUploadResponse, NoUpdate )

                ( True, Nothing ) ->
                    ( model, Cmd.none, NoUpdate )

                -- File deleted?
                ( False, _ ) ->
                    ( model, pushUrl sharedState.navKey (reverseRoute <| CourseDetailRoute model.course_id), NoUpdate )

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
            ( model, pushUrl sharedState.navKey (reverseRoute <| CourseDetailRoute model.course_id), NoUpdate )

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
        , viewDeleteMaterialDialog sharedState model
        , normalPage
            [ viewFormLoadingOrError sharedState model ]
        ]


viewFormLoadingOrError : SharedState -> Model -> Html Msg
viewFormLoadingOrError sharedState model =
    case model.materialResponse of
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
                if model.createMaterial then
                    "Blatt erstellen"

                else
                    "Blatt bearbeiten"
            ]
        , rRow
            [ fileUploader model.hover model.file DragEnter DragLeave Pick GotFiles ]
        , rRowExtraSpacing <|
            r1Column <|
                inputElement
                    { label = "Material Name"
                    , placeholder = "Name"
                    , fieldType = "text"
                    , value = model.name
                    }
                    Name
                    model.errors
                    SetField
        , rRow <|
            r2Column
                [ inputLabel "Material Type"
                , multiButton
                    [ ( "Slide", model.materialType == Slide, SetMaterialType Slide )
                    , ( "Supplementary", model.materialType == Supplementary, SetMaterialType Supplementary )
                    ]
                ]
                [ inputLabel "Required Role"
                , multiButton
                    [ ( "Student", model.requiredRole == 0, SetRequiredRole 0 )
                    , ( "Tutor", model.requiredRole == 1, SetRequiredRole 1 )
                    , ( "Admin", model.requiredRole == 2, SetRequiredRole 2 )
                    ]
                ]
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
                    { label = "Lecture date"
                    , value = model.lectureAtDate
                    , datePicker = model.lectureDatePicker
                    , settings = datePickerSettings sharedState
                    }
                    LectureDate
                    model.errors
                    LectureDatePickerMsg
                )
                (timeInputElement
                    { label = "Lecture time"
                    , placeholder = "Select time..."
                    , timePicker = model.lectureTimePicker
                    , settings = timePickerSettings
                    }
                    LectureTime
                    model.errors
                    LectureTimePickerMsg
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
                    (if model.createMaterial then
                        "Erstellen"

                     else
                        "Bearbeiten"
                    )
                    (if model.createMaterial then
                        Create

                     else
                        Update
                    )
        ]
            ++ (if not model.createMaterial then
                    [ rRowButton <|
                        PbbButton <|
                            PbbActive
                                "Löschen"
                                RequestDelete
                    ]

                else
                    []
               )


viewDeleteMaterialDialog : SharedState -> Model -> Html Msg
viewDeleteMaterialDialog sharedState model =
    simpleDialog
        "Delete the Material?"
        "Are you sure you want to delete the material? This cannot be undone. The material and everything associated with the material like files are gone."
        [ ( "Delete", Styles.buttonRedStyle, Delete )
        , ( "Cancel", Styles.buttonGreenStyle, DeleteMaterialDialogShown False )
        ]
        model.deleteMaterialDialogState
        deleteMaterialDialogConfig


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
    | LectureTime
    | LectureDate
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
                , lecturePosix = testIfTimeIsReady model.lectureAtDate model.lectureAtTime newPos
                , publishedPosix = testIfTimeIsReady model.publishedAtDate model.publishedAtTime newPos
            }

        _ ->
            -- times are set by TimePicker
            model


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .name ( Name, "Bitte gib einen Blattnamen ein." )
        , ifNothing .publishedAtTime ( PublishedTime, "Bitte gib eine Freigabezeit ein." )
        , ifNothing .publishedAtDate ( PublishedDate, "Bitte gib ein Freigabedatum ein." )
        , ifNothing .lectureAtTime ( LectureTime, "Bitte gib eine Vorlesungszeit ein." )
        , ifNothing .lectureAtDate ( LectureDate, "Bitte gib ein Vorlesungsdatum ein." )
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


deleteMaterialDialogConfig : Dialog.Config Msg
deleteMaterialDialogConfig =
    Dialog.Config
        Styles.dialogVisibleStyle
        Styles.dialogGoneStyle
        DeleteMaterialDialogShown
        True
        NoOp
