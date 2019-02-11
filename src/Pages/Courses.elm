{-
    This is the course site. Here, all courses are listed.
        - New courses can be created by root users.
        - Users can enroll in a course
        - Users can be removed from the course 
        - Courses are split between current and past (archive)
-}
module Pages.Courses exposing (..)

import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import RemoteData exposing (RemoteData(..), WebData)
import Utils.DateFormatter as DF
import Time
import I18n
import Api.Data.Course exposing (Course)

type alias Model =
    { courseProgress : WebData (List Course)
    , enrollProgress : WebData String
    , disenrollProgress : WebData String
    }

type Msg
    = CourseResponse (WebData (List Course))
    | Enroll Course
    | Disenroll Course
    | EnrollResponse (WebData String)
    | DisenrollResponse (WebData String)
    | NavigateTo Route
    

init : (Model, Cmd Msg)
init = 
    ( 
        { courseProgress = RemoteData.Success 
            [ 
                { id = 0
                , name = "Informatik I"
                , description = Just 
                """
                Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
                tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim 
                veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea 
                commodo consequat. Duis aute irure dolor in reprehenderit in voluptate 
                velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
                occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
                mollit anim id est laborum.
                """
                , begins_at = Time.millisToPosix 1549888135000
                , ends_at = Time.millisToPosix 1560256135000
                , required_points = Just 250
                , sheets = Nothing
                , materials = Nothing 
                }
            ,   
                { id = 1
                , name = "Informatik II"
                , description = Just 
                """
                Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
                tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim 
                veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea 
                commodo consequat. Duis aute irure dolor in reprehenderit in voluptate 
                velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
                occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
                mollit anim id est laborum.
                """
                , begins_at = Time.millisToPosix 1554985735000
                , ends_at = Time.millisToPosix 1570796935000
                , required_points = Nothing
                , sheets = Nothing
                , materials = Nothing 
                }
            ,   
                { id = 1
                , name = "ML"
                , description = Just 
                """
                Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
                tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim 
                veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea 
                commodo consequat. Duis aute irure dolor in reprehenderit in voluptate 
                velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
                occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
                mollit anim id est laborum.
                """
                , begins_at = Time.millisToPosix 1528720135000
                , ends_at = Time.millisToPosix 1541939335000
                , required_points = Nothing
                , sheets = Nothing
                , materials = Nothing 
                }
            ]
        , enrollProgress = NotAsked
        , disenrollProgress = NotAsked
        }
    , Cmd.none
    )

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        CourseResponse response ->
            ({model | courseProgress = response }, Cmd.none, NoUpdate)
        
        Enroll course ->
            (model, Cmd.none, NoUpdate)
        
        Disenroll course ->
            (model, Cmd.none, NoUpdate)

        EnrollResponse response ->
            (model, Cmd.none, NoUpdate)

        DisenrollResponse response ->
            (model, Cmd.none, NoUpdate)

        NavigateTo route ->
            (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        translate = 
            I18n.get sharedState.translations
    in
    case model.courseProgress of
        RemoteData.Success courses ->
            let 
                currentTime = Maybe.withDefault 
                    (Time.millisToPosix 0) -- THIS SHOULD NEVER HAPPEN!
                    sharedState.currentTime

                currentCourses = courses
                    |> List.filter (\course -> 
                        Time.posixToMillis course.ends_at
                        |> (<) (Time.posixToMillis currentTime)
                    )
                    |> List.map (\course -> viewRenderCourse sharedState course)
                
                oldCourses = courses
                    |> List.filter (\course -> 
                        Time.posixToMillis course.ends_at
                        |> (>) (Time.posixToMillis currentTime)
                    )
                    |> List.map (\course -> viewRenderCourse sharedState course)

                cTemp = 
                    [ text "Aktuell"
                    ] ++ currentCourses
                
                content = 
                    if List.length oldCourses > 0 then
                        cTemp ++ 
                            [ text "Archiv"
                            ] ++ oldCourses
                    else
                        cTemp
            in
            div [] content

        _ ->
            div [] []
        

viewRenderCourse : SharedState -> Course -> Html Msg
viewRenderCourse sharedState course =
    div []
        [ text course.name -- Bold header
        , text <| Maybe.withDefault "" course.description -- Normal paragraph
        , text "Beginn"
        , DF.fullDateFormatter sharedState course.begins_at
        , text "Ende"
        , DF.fullDateFormatter sharedState course.ends_at
        , button [ ] [ text "Enroll" ] -- TODO check if user is enrolled or not. Either show and execute enroll or disenroll
        ]