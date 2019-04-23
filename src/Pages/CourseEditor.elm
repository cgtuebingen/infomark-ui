{-
   This is the course creation page
-}


module Pages.CourseEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Course exposing (Course)
import Api.Request.Courses as CoursesRequest
import Browser.Navigation exposing (back, pushUrl)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbState(..)
        , dateInputElement
        , inputElement
        , normalPage
        , pageContainer
        , r1Column
        , r2Column
        , rContainer
        , rRow
        , rRowButton
        , rRowHeader
        , textAreaElement
        , viewFormErrors
        )
import Components.Toasty
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Time
import Toasty
import Utils.DateAndTimeUtils as DTU
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors)
import Validate exposing (Validator, ifBlank, ifNothing, validate)


type Msg
    = NavigateTo Route
    | CourseGetResponse (WebData Course)
    | CourseCreateResponse (WebData Course)
    | CourseEditResponse (WebData ())
    | SetField Field String
    | BeginDatePicker DatePicker.Msg
    | EndDatePicker DatePicker.Msg
    | PointsRequiredToggle
    | CreateOrEdit
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)


type alias Model =
    { id : Int
    , courseName : String
    , description : String
    , required_percentage : Maybe String
    , errors : List Error
    , getCourseResponse : WebData Course
    , beginsAtDate : Maybe Date
    , endsAtDate : Maybe Date
    , beginsAtDatepicker : DatePicker.DatePicker
    , endsAtDatepicker : DatePicker.DatePicker
    , createCourse : Bool
    , toasties : Toasty.Stack Components.Toasty.Toast
    }


settings : SharedState -> DatePicker.Settings
settings sharedState =
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


initModel : ( Model, Cmd Msg )
initModel =
    let
        ( beginDatePicker, beginDatePickerFx ) =
            DatePicker.init

        ( endDatePicker, endDatePickerFx ) =
            DatePicker.init
    in
    ( { id = 0
      , courseName = ""
      , description = ""
      , required_percentage = Nothing
      , errors = []
      , getCourseResponse = NotAsked
      , beginsAtDatepicker = beginDatePicker
      , endsAtDatepicker = endDatePicker
      , beginsAtDate = Nothing
      , endsAtDate = Nothing
      , createCourse = True
      , toasties = Toasty.initialState
      }
    , Cmd.batch
        [ Cmd.map BeginDatePicker beginDatePickerFx
        , Cmd.map EndDatePicker endDatePickerFx
        ]
    )


initCreate : ( Model, Cmd Msg )
initCreate =
    let
        ( model, cmd ) =
            initModel
    in
    ( model
    , cmd
    )


initEdit : Int -> ( Model, Cmd Msg )
initEdit id =
    let
        ( model, cmd ) =
            initModel
    in
    ( { model | getCourseResponse = Loading, createCourse = False, id = id }
    , Cmd.batch
        [ cmd
        , CoursesRequest.courseGet id CourseGetResponse
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        CourseGetResponse (RemoteData.Success course) ->
            let
                new_model =
                    fillModelFromResponse sharedState course model
            in
            ( { new_model | getCourseResponse = NotAsked }
            , Cmd.none
            , NoUpdate
            )

        CourseGetResponse response ->
            ( { model | getCourseResponse = response }, Cmd.none, NoUpdate )

        PointsRequiredToggle ->
            ( { model
                | required_percentage =
                    if model.required_percentage == Nothing then
                        Just "0"

                    else
                        Nothing
              }
            , Cmd.none
            , NoUpdate
            )

        CreateOrEdit ->
            case validate (modelValidator sharedState) model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    let
                        body =
                            fillRequestFromModel model
                    in
                    ( { model | errors = [] }
                    , case model.createCourse of
                        True ->
                            CoursesRequest.coursesPost body CourseCreateResponse

                        False ->
                            CoursesRequest.coursePut model.id body CourseEditResponse
                    , NoUpdate
                    )

        CourseCreateResponse response ->
            updateHandleCreateOrEditResponse sharedState model response

        CourseEditResponse response ->
            updateHandleCreateOrEditResponse sharedState model response

        -- Start rest request
        BeginDatePicker subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (settings sharedState) subMsg model.beginsAtDatepicker

                newDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.beginsAtDate
            in
            ( { model
                | beginsAtDate = newDate
                , beginsAtDatepicker = newDatePicker
              }
            , Cmd.none
            , NoUpdate
            )

        EndDatePicker subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (settings sharedState) subMsg model.endsAtDatepicker

                newDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.endsAtDate
            in
            ( { model
                | endsAtDate = newDate
                , endsAtDatepicker = newDatePicker
              }
            , Cmd.none
            , NoUpdate
            )

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )


updateHandleCreateOrEditResponse : SharedState -> Model -> WebData ret -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleCreateOrEditResponse sharedState model response =
    case response of
        Success _ ->
            ( model, back sharedState.navKey 1, NoUpdate )

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



-- TODO handle loading


view : SharedState -> Model -> Html Msg
view sharedState model =
    pageContainer
        [ Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
        , normalPage
            [ viewFormLoadingOrError sharedState model ]
        ]


viewFormLoadingOrError : SharedState -> Model -> Html Msg
viewFormLoadingOrError sharedState model =
    case model.getCourseResponse of
        Loading ->
            -- Display spinner
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
    rContainer
        [ rRowHeader <|
            if model.createCourse then
                "Kurs erstellen"

            else
                "Kurs bearbeiten"
        , rRow <|
            r1Column <|
                inputElement
                    { label = "Course Name"
                    , placeholder = "Name"
                    , fieldType = "text"
                    , value = model.courseName
                    }
                    Name
                    model.errors
                    SetField
        , rRow <|
            r2Column
                (dateInputElement
                    { label = "Start"
                    , value = model.beginsAtDate
                    , datePicker = model.beginsAtDatepicker
                    , settings = settings sharedState
                    }
                    BeginsAtDate
                    model.errors
                    BeginDatePicker
                )
                (dateInputElement
                    { label = "Ende"
                    , value = model.endsAtDate
                    , datePicker = model.endsAtDatepicker
                    , settings = settings sharedState
                    }
                    EndsAtDate
                    model.errors
                    EndDatePicker
                )
        , rRow <|
            r1Column <|
                textAreaElement
                    { label = "Beschreibung"
                    , placeholder = "Beschreibung"
                    , value = model.description
                    , rows = 1
                    }
                    Description
                    model.errors
                    SetField
        , rRow
            [ div [ classes [ TC.h3, TC.flex, TC.justify_between, TC.items_center ] ] <|
                viewRequiredPercentage model
            , viewFormErrors RequiredPercentage model.errors
            ]
        , rRowButton <|
            PbbButton <|
                PbbActive
                    (if model.createCourse then
                        "Erstellen"

                     else
                        "Bearbeiten"
                    )
                    CreateOrEdit
        ]


viewRequiredPercentage : Model -> List (Html Msg)
viewRequiredPercentage model =
    let
        buttonText =
            if model.required_percentage == Nothing then
                "No percentage required"

            else
                "Percentage required"

        showElement =
            if model.required_percentage == Nothing then
                text ""

            else
                input
                    [ type_ "number"
                    , Styles.lineInputStyle
                    , classes [ TC.w_10, TC.tc ]
                    , placeholder "Percentage"
                    , onInput <| SetField RequiredPercentage
                    , value <| Maybe.withDefault "0" model.required_percentage
                    ]
                    []
    in
    [ button
        [ Styles.buttonRedStyle
        , classes [ TC.br_pill, TC.ph4, TC.pv3 ]
        , onClick PointsRequiredToggle
        ]
        [ text buttonText ]
    , showElement
    ]


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )


fillModelFromResponse : SharedState -> Course -> Model -> Model
fillModelFromResponse sharedState course model =
    let
        timezone =
            Maybe.withDefault Time.utc sharedState.timezone

        beginDate =
            Date.fromPosix timezone course.begins_at

        endDate =
            Date.fromPosix timezone course.ends_at
    in
    { model
        | courseName = course.name
        , description = course.description
        , beginsAtDate = Just <| beginDate
        , endsAtDate = Just <| endDate
        , required_percentage =
            (\num ->
                if num == 0 then
                    Nothing

                else
                    Just <| String.fromInt num
            )
                course.required_percentage
    }


fillRequestFromModel : Model -> Course
fillRequestFromModel model =
    let
        -- Ugly as sin. Should never happen because we validate the model before
        defaultPosix =
            Time.millisToPosix 0

        defaultDate =
            Date.fromPosix Time.utc defaultPosix

        beginDate =
            Maybe.withDefault defaultDate model.beginsAtDate

        endDate =
            Maybe.withDefault defaultDate model.endsAtDate

        beginPosix =
            DTU.dateToPosix beginDate |> Result.toMaybe |> Maybe.withDefault defaultPosix

        endPosix =
            DTU.dateToPosix endDate |> Result.toMaybe |> Maybe.withDefault defaultPosix
    in
    { id = model.id
    , name = model.courseName
    , description = model.description
    , begins_at = beginPosix
    , ends_at = endPosix
    , required_percentage =
        Maybe.withDefault 0 <|
            String.toInt <|
                Maybe.withDefault "0" model.required_percentage
    }


type Field
    = Name
    | Description
    | RequiredPercentage
    | BeginsAtDate
    | EndsAtDate


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Name ->
            { model | courseName = value }

        Description ->
            { model | description = value }

        RequiredPercentage ->
            { model | required_percentage = Just value }

        _ ->
            -- Only date fields left. They are set by the date picker
            model


modelValidator : SharedState -> Validator Error Model
modelValidator sharedState =
    Validate.all
        [ ifBlank .courseName ( Name, "Bitte gib einen Kursnamen ein." )
        , ifBlank .description ( Description, "Bitte gib eine Kursbeschreibung ein." )
        , Validate.firstError
            [ ifNothing .beginsAtDate ( BeginsAtDate, "Setze ein Startdatum." )
            , ifFirstDateNotOlder .beginsAtDate .endsAtDate ( BeginsAtDate, "Das Startdatum muss vor dem Enddatum liegen." )
            ]
        , Validate.firstError
            [ ifNothing .endsAtDate ( EndsAtDate, "Setze ein Enddatum." )
            , ifDateNotInFuture sharedState .endsAtDate ( EndsAtDate, "Das Datum darf nicht in der Vergangenheit liegen." )
            , ifFirstDateNotOlder .beginsAtDate .endsAtDate ( EndsAtDate, "Das Enddatum muss nach dem Startdatum liegen." )
            ]
        , Validate.firstError
            [ ifJustAndNotInt .required_percentage ( RequiredPercentage, "Die benötigten Prozent müssen eine Prozentzahl sein." )
            , ifIsNotPercentage .required_percentage ( RequiredPercentage, "Die benötigten Prozent müssen zwischen 0 und 100% liegen." )
            ]
        ]


ifFirstDateNotOlder : (subject -> Maybe Date) -> (subject -> Maybe Date) -> error -> Validator error subject
ifFirstDateNotOlder subject1ToMaybeDate subject2ToMaybeDate error =
    Validate.ifFalse (\subject -> isFirstDateOlder (subject1ToMaybeDate subject) (subject2ToMaybeDate subject)) error


isFirstDateOlder : Maybe Date -> Maybe Date -> Bool
isFirstDateOlder maybeFirstDate maybeSecondDate =
    case ( maybeFirstDate, maybeSecondDate ) of
        ( Just firstDate, Just secondDate ) ->
            case Date.compare firstDate secondDate of
                LT ->
                    True

                _ ->
                    False

        ( _, _ ) ->
            False


ifDateNotInFuture : SharedState -> (subject -> Maybe Date) -> error -> Validator error subject
ifDateNotInFuture sharedState subjectToMaybeDate error =
    Validate.ifFalse (\subject -> isDateInFuture sharedState (subjectToMaybeDate subject)) error


isDateInFuture : SharedState -> Maybe Date -> Bool
isDateInFuture sharedState maybeDate =
    let
        currentTimezone =
            Maybe.withDefault Time.utc sharedState.timezone

        currentDate =
            Date.fromPosix currentTimezone <|
                Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime
    in
    case maybeDate of
        Just date ->
            case Date.compare date currentDate of
                LT ->
                    False

                EQ ->
                    True

                GT ->
                    True

        Nothing ->
            False


ifJustAndNotInt : (subject -> Maybe String) -> error -> Validator error subject
ifJustAndNotInt subjectToMaybeString error =
    Validate.ifFalse (\subject -> isNothingOrInt (subjectToMaybeString subject)) error


isNothingOrInt : Maybe String -> Bool
isNothingOrInt maybeString =
    case maybeString of
        Just string ->
            case String.toInt string of
                Nothing ->
                    False

                Just _ ->
                    True

        Nothing ->
            True


ifIsNotPercentage : (subject -> Maybe String) -> error -> Validator error subject
ifIsNotPercentage subjectToMaybeString error =
    Validate.ifFalse (\subject -> isPercentageRequiredAndOk (subjectToMaybeString subject)) error


isPercentageRequiredAndOk : Maybe String -> Bool
isPercentageRequiredAndOk maybePercentage =
    case maybePercentage of
        Just percentage ->
            case String.toInt percentage of
                Nothing ->
                    False

                Just perc ->
                    if perc >= 0 && perc <= 100 then
                        True

                    else
                        False

        Nothing ->
            True
