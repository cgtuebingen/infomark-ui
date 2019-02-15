{-
   This is the course creation page
-}


module Pages.CourseEditor exposing (Model, Msg(..), initCreate, initEdit, update, view)

import Api.Data.Course exposing (Course)
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
import Time exposing (Posix)
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Utils.Styles as Styles
import Api.Data.Course as Course
import Api.Request.Courses as CoursesRequest


type Msg
    = NavigateTo Route
    | CourseGetResponse (WebData Course)
    | SetField Field String
    | ToDatePicker DatePicker.Msg


type alias Model =
    { courseName : String
    , description : String
    , begins_at : Maybe Posix
    , ends_at : Maybe Posix
    , required_points : Maybe Int
    , errors : List Error
    , getCourseResponse : WebData Course
    }


type alias Error =
    ( Field, String )

initModel : Model
initModel =
    { courseName = ""
    , description = ""
    , begins_at = Nothing
    , ends_at = Nothing
    , required_points = Nothing
    , errors = []
    , getCourseResponse = NotAsked
    }

initCreate : ( Model, Cmd Msg )
initCreate =
    ( initModel
    , Cmd.none )


initEdit : Int -> ( Model, Cmd Msg )
initEdit id =
    ( {initModel | getCourseResponse = Loading}, CoursesRequest.courseGet id CourseGetResponse )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        CourseGetResponse (RemoteData.Success course) ->
            let
                new_model = fillModelFromResponse course model
            in
            
            ( { new_model | getCourseResponse = NotAsked}
            , Cmd.none, NoUpdate)

        CourseGetResponse response ->
            ({ model | getCourseResponse = response }, Cmd.none, NoUpdate)

        _ ->
            (model, Cmd.none, NoUpdate)


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
        [ classes [TC.w_100]] 
        [ h1 [ Styles.headerStyle ] [ text "Kurs erstellen"]
        , div [ classes [TC.mt3, TC.cf, TC.ph2_ns] ] 
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                inputElement "Course Name" "Name" "text" Name model.courseName model.errors
            ]
        , div [ classes [TC.mt3, TC.cf, TC.ph2_ns] ] 
            [ label 
                [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
                , Styles.labelStyle 
                ] [ text "Beschreibung" ]
            , textarea 
                [ Styles.inputStyle
                , classes [TC.w_100]
                , rows 10
                , placeholder "Beschreibung"
                , onInput <| SetField Description
                ] []
            ]
        
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
        , Styles.inputStyle
        , classes [ TC.w_100 ]
        , placeholder inputPlaceholder
        , onInput <| SetField field
        , value curVal
        ]
        []
    , viewFormErrors field errors
    ]


fillModelFromResponse : Course -> Model -> Model
fillModelFromResponse course model =
    { model | 
        courseName = course.name,
        description = Maybe.withDefault "" course.description,
        begins_at = Just course.begins_at,
        ends_at = Just course.ends_at,
        required_points = course.required_points 
    }

type Field
    = Name
    | Description
    | RequiredPoints


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Name ->
            { model | courseName = value }

        Description ->
            { model | description = value }

        RequiredPoints ->
            { model | required_points = String.toInt value }


viewFormErrors : Field -> List Error -> Html Msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [ TC.red ] ] [ text error ])
        |> ul [ classes [ TC.list, TC.pl0, TC.center ] ]