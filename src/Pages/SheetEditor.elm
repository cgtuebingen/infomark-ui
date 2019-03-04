module Pages.SheetEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Sheet exposing (Sheet)
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Array
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors, perform)
import Utils.DateFormatter as DF
import Utils.DateAndTimeUtils as DTU
import TimePicker exposing (TimeEvent(..), TimePicker)
import Components.CommonElements exposing (inputElement, timeInputElement, dateInputElement, sliderInputElement)


type Msg
    = NavigateTo Route
    | PublishedTimePickerMsg TimePicker.Msg
    | PublishedDatePickerMsg DatePicker.Msg
    | DeadlineTimePickerMsg TimePicker.Msg
    | DeadlineDatePickerMsg DatePicker.Msg
    | GetRealOffset
    | SetField Field String


type alias Model =
    { id : Int
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
    , errors : List Error
    }


initModel : (Model, Cmd Msg)
initModel =
    let
        ( publishedDatePicker, publishedDatePickerFx ) =
            DatePicker.init

        ( deadlineDatePicker, deadlineDatePickerFx ) =
            DatePicker.init
    in
    ({ id = 0
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
    , sheetResponse = NotAsked
    , createSheet = True
    , utcOffsetPos = DTU.utcZeroOffsetIndex
    , errors = []
    }
    , Cmd.batch
        [ Cmd.map PublishedDatePickerMsg publishedDatePickerFx
        , Cmd.map DeadlineDatePickerMsg deadlineDatePickerFx
        , perform GetRealOffset
        ]
    )


initCreate : ( Model, Cmd Msg )
initCreate = initModel


initEdit : Int -> ( Model, Cmd Msg )
initEdit id =
    let
        ( model, cmd ) =
            initModel
    in
    ( 
    { model
        | createSheet = False
    }
    , cmd
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )

        PublishedTimePickerMsg subMsg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update timePickerSettings subMsg model.publishedTimePicker

                newTime = case timeEvent of
                    NoChange -> model.publishedAtTime
                    Changed time -> time
            in
            ( { model 
                | publishedTimePicker = updatedPicker
                , publishedAtTime = newTime
                , publishedPosix = testIfTimeIsReady model.publishedAtDate model.publishedAtTime model.utcOffsetPos }, Cmd.none, NoUpdate )

        PublishedDatePickerMsg subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (datePickerSettings sharedState) subMsg model.publishedDatePicker

                newDate = case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.publishedAtDate
            in
            ( { model
                | publishedAtDate = newDate
                , publishedDatePicker = newDatePicker
                , publishedPosix = testIfTimeIsReady model.publishedAtDate model.publishedAtTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        DeadlineTimePickerMsg subMsg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update timePickerSettings subMsg model.deadlineTimePicker

                newTime = case timeEvent of
                    NoChange -> model.deadlineAtTime
                    Changed time -> time
            in
            ( { model 
                | deadlineTimePicker = updatedPicker
                , deadlineAtTime = newTime
                , deadlinePosix = testIfTimeIsReady model.deadlineAtDate model.deadlineAtTime model.utcOffsetPos
               }, Cmd.none, NoUpdate)

        DeadlineDatePickerMsg subMsg ->
            let
                ( newDatePicker, event ) =
                    DatePicker.update (datePickerSettings sharedState) subMsg model.deadlineDatePicker

                newDate = case event of
                        Picked date ->
                            Just date

                        _ ->
                            model.deadlineAtDate
            in
            ( { model
                | deadlineAtDate = newDate
                , deadlineDatePicker = newDatePicker
                , deadlinePosix = testIfTimeIsReady model.deadlineAtDate model.deadlineAtTime model.utcOffsetPos
              }
            , Cmd.none
            , NoUpdate
            )

        GetRealOffset ->
            ( 
                { model 
                    | utcOffsetPos = 
                        Maybe.withDefault DTU.utcZeroOffsetIndex <|
                            DTU.timeZoneToIndex <| 
                                Maybe.withDefault Time.utc sharedState.timezone
                }
            , Cmd.none
            , NoUpdate 
            )
            

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )


joinTime : Date -> TimePicker.Time -> Int -> Time.Posix
joinTime date time utcOffsetPos =
    let
        offsetPartsArray = Array.fromList DTU.utcOffsetsPartsList
        offset = Maybe.withDefault {multiplier = 1, hours = 0, minutes = 0} <|
            Array.get utcOffsetPos offsetPartsArray
    in
    DTU.joinDateTimeAndOffset date time offset
    

testIfTimeIsReady : Maybe Date -> Maybe TimePicker.Time -> Int -> Maybe Time.Posix
testIfTimeIsReady maybeDate maybeTime offset =
    case (maybeDate, maybeTime) of
        (Just date, Just time) -> Just <| joinTime date time offset
        (_, _) -> Nothing


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
    case model.sheetResponse of
        Loading -> -- Display Spinner
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
        offsetLabelsArray = Array.fromList DTU.utcOffsetLabelsList
    in
    div
        [ classes [ TC.w_100 ] ]
        [ h1 
            [ Styles.headerStyle ] 
            [ text <|
                if model.createSheet then "Blatt erstellen" else "Blatt bearbeiten" 
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                inputElement 
                    { label = "Sheet Name"
                    , placeholder = "Name"
                    , fieldType = "text"
                    , value = model.name } Name model.errors SetField
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ] <|
                dateInputElement
                    { label = "Published date"
                    , value = model.publishedAtDate
                    , datePicker = model.publishedDatePicker
                    , settings = (datePickerSettings sharedState) 
                    } PublishedDate model.errors PublishedDatePickerMsg
            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ] <|
                timeInputElement
                    { label = "Published time"
                    , placeholder = "Select time..."
                    , timePicker = model.publishedTimePicker
                    , settings = timePickerSettings 
                    } PublishedTime model.errors PublishedTimePickerMsg
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ] <|
                dateInputElement
                    { label = "Deadline date"
                    , value = model.deadlineAtDate
                    , datePicker = model.deadlineDatePicker
                    , settings = (datePickerSettings sharedState) 
                    } DeadlineDate model.errors DeadlineDatePickerMsg
            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ] <|
                timeInputElement
                    { label = "Deadline time"
                    , placeholder = "Select time..."
                    , timePicker = model.deadlineTimePicker
                    , settings = timePickerSettings 
                    } DeadlineTime model.errors DeadlineTimePickerMsg
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                sliderInputElement
                    { label = "UTC Offset"
                    , value = model.utcOffsetPos
                    , min = 0
                    , max = (Array.length offsetLabelsArray) - 1
                    , step = 1
                    , valueLabel = Maybe.withDefault "Z" <| Array.get model.utcOffsetPos offsetLabelsArray
                    } UtcOffset model.errors SetField
            ]
        ]
   

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
    let
        _ = Debug.log "setField" (field, value)
    in
    
    case field of
        Name ->
            { model | name = value }

        UtcOffset ->
            let
                newPos = Maybe.withDefault DTU.utcZeroOffsetIndex <| 
                    String.toInt value
            in
            
            { model 
                | utcOffsetPos = newPos
                , deadlinePosix = testIfTimeIsReady model.deadlineAtDate model.deadlineAtTime newPos
                , publishedPosix = testIfTimeIsReady model.publishedAtDate model.publishedAtTime newPos
            }


        _ -> -- times are set by TimePicker
            model