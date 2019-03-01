module Pages.SheetEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Sheet exposing (Sheet)
import Browser.Navigation exposing (pushUrl)
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
import Time
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors)
import Utils.DateFormatter as DF
import Utils.DateAndTimeUtils as DTU
import TimePicker exposing (TimeEvent(..), TimePicker)
import Components.CommonElements exposing (inputElement)


type Msg
    = NavigateTo Route
    | PublishedTimePickerMsg TimePicker.Msg
    | DeadlineTimePickerMsg TimePicker.Msg
    | SetField Field String


type alias Model =
    { id : Int
    , name : String
    , publishedTimePicker : TimePicker
    , deadlineTimePicker : TimePicker
    , sheetResponse : WebData Sheet
    , createSheet : Bool
    , errors : List Error
    }


initModel : Model
initModel =
    { id = 0
    , name = ""
    , publishedTimePicker = TimePicker.init Nothing
    , deadlineTimePicker = TimePicker.init Nothing
    , sheetResponse = NotAsked
    , createSheet = True
    , errors = []
    }


initCreate : ( Model, Cmd Msg )
initCreate =
    ( 
    initModel
    , Cmd.none 
    )


initEdit : Int -> ( Model, Cmd Msg )
initEdit id =
    ( 
    { initModel
        | createSheet = False
    }
    , Cmd.none 
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
            in
            ( { model | publishedTimePicker = updatedPicker }, Cmd.none, NoUpdate )

        DeadlineTimePickerMsg subMsg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update timePickerSettings subMsg model.deadlineTimePicker
            in
            ( { model | deadlineTimePicker = updatedPicker }, Cmd.none, NoUpdate)

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )


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
        {-[ h3 [] [ text "Sheeeeet" ]
        , div [ class "default-time-picker" ]
            [ Html.map PublishedTimePickerMsg <| TimePicker.view timePickerSettings model.publishedTimePicker 
            ]
        ]-}


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


        ]


timePickerSettings : TimePicker.Settings
timePickerSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
        { defaultSettings | showSeconds = False, minuteStep = 15, use24Hours = True }


type alias Error =
    ( Field, String )

type Field
    = Name


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Name ->
            { model | name = value }