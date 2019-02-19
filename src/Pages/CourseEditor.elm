{-
   This is the course creation page
-}


module Pages.CourseEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Course as Course exposing (Course)
import Api.Request.Courses as CoursesRequest
import Browser.Navigation exposing (pushUrl)
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
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
import Time exposing (Posix)
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Validate exposing (Validator, ifBlank, ifNotInt, ifNothing, ifTrue, validate)


type Msg
    = NavigateTo Route
    | CourseGetResponse (WebData Course)
    | SetField Field String
    | BeginDatePicker DatePicker.Msg
    | EndDatePicker DatePicker.Msg
    | PointsRequiredToggle
    | CreateOrEdit


type alias Model =
    { courseName : String
    , description : String
    , required_percentage : Maybe String
    , errors : List Error
    , getCourseResponse : WebData Course
    , beginsAtDate : Maybe Date
    , endsAtDate : Maybe Date
    , beginsAtDatepicker : DatePicker.DatePicker
    , endsAtDatepicker : DatePicker.DatePicker
    , createCourse : Bool
    }


settings : SharedState -> DatePicker.Settings
settings sharedState =
    let
        curTime =
            Maybe.withDefault (Time.millisToPosix 0) sharedState.currentTime
    in
    { defaultSettings
        | inputAttributes = [ Styles.lineInputStyle, classes [ TC.w_100, TC.mb3 ] ]
        , dateFormatter = DF.dateToShortFormatString sharedState
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
    ( { courseName = ""
      , description = ""
      , required_percentage = Nothing
      , errors = []
      , getCourseResponse = NotAsked
      , beginsAtDatepicker = beginDatePicker
      , endsAtDatepicker = endDatePicker
      , beginsAtDate = Nothing
      , endsAtDate = Nothing
      , createCourse = True
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
    ( { model | getCourseResponse = Loading, createCourse = False }
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
                    ( { model | errors = [] }, Cmd.none, NoUpdate )

        -- Start rest request
        BeginDatePicker subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (settings sharedState) subMsg model.beginsAtDatepicker
            in
            ( { model
                | beginsAtDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.beginsAtDate
                , beginsAtDatepicker = newDatePicker
              }
            , Cmd.none
            , NoUpdate
            )

        EndDatePicker subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (settings sharedState) subMsg model.endsAtDatepicker
            in
            ( { model
                | endsAtDate =
                    case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.endsAtDate
                , endsAtDatepicker = newDatePicker
              }
            , Cmd.none
            , NoUpdate
            )



--_ ->
--    (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0_ns, TC.w_100 ] ]
        [ div
            [ classes
                [ TC.mw8
                , TC.ph4
                , TC.ph5_ns
                , TC.center
                ]
            ]
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
    div
        [ classes [ TC.w_100 ] ]
        [ h1 [ Styles.headerStyle ] [ text "Kurs erstellen" ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                inputElement "Course Name" "Name" "text" Name model.courseName model.errors
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            -- Second Row (Start date, End date)
            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ]
                -- First element
                [ label
                    [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
                    , Styles.labelStyle
                    ]
                    [ text "Start" ]
                , DatePicker.view model.beginsAtDate (settings sharedState) model.beginsAtDatepicker
                    |> Html.map BeginDatePicker
                , viewFormErrors BeginsAtDate model.errors
                ]
            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
                -- Second element
                [ label
                    [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
                    , Styles.labelStyle
                    ]
                    [ text "Ende" ]
                , DatePicker.view model.endsAtDate (settings sharedState) model.endsAtDatepicker
                    |> Html.map EndDatePicker
                , viewFormErrors EndsAtDate model.errors
                ]
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ label
                [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
                , Styles.labelStyle
                ]
                [ text "Beschreibung" ]
            , textarea
                (List.append Styles.textAreaReset
                    [ Styles.lineInputStyle
                    , classes [ TC.w_100 ]
                    , rows 10
                    , placeholder "Beschreibung"
                    , onInput <| SetField Description
                    ]
                )
                []
            , viewFormErrors Description model.errors
            ]
        , div [ classes [ TC.mt3, TC.ph2_ns ] ]
            [ div [ classes [ TC.h3, TC.flex, TC.justify_between, TC.items_center ] ] <|
                viewRequiredPercentage model
            , viewFormErrors RequiredPercentage model.errors
            ]
        , button
            [ Styles.buttonGreyStyle
            , classes [ TC.mt4, TC.w_100 ]
            , onClick CreateOrEdit
            ]
            [ text "Erstellen" ]

        -- TODO check if edit or create AND use translations
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


inputElement : String -> String -> String -> Field -> String -> List Error -> List (Html Msg)
inputElement inputLabel inputPlaceholder fieldType field curVal errors =
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputLabel
        ]
    , input
        [ type_ fieldType
        , Styles.lineInputStyle
        , classes [ TC.w_100, TC.mb3 ]
        , placeholder inputPlaceholder
        , onInput <| SetField field
        , value curVal
        ]
        []
    , viewFormErrors field errors
    ]


fillModelFromResponse : SharedState -> Course -> Model -> Model
fillModelFromResponse sharedState course model =
    let
        timezone =
            Maybe.withDefault Time.utc sharedState.timezone
    in
    { model
        | courseName = course.name
        , description = Maybe.withDefault "" course.description
        , beginsAtDate = Just <| Date.fromPosix timezone course.begins_at
        , endsAtDate = Just <| Date.fromPosix timezone course.ends_at
        , required_percentage = Maybe.map String.fromInt course.required_percentage
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
            model



-- Only date fields left. They are set by the date picker


viewFormErrors : Field -> List Error -> Html Msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [ TC.red ] ] [ text error ])
        |> ul [ classes [ TC.list, TC.pl0, TC.center ] ]


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
    let
        _ =
            Debug.log "isNothingOrInt" maybeString
    in
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
    let
        _ =
            Debug.log "isPercRequiredAndOk" maybePercentage
    in
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
