{-
   This is the course detail page. It displays:
   - Basic information (Title, Description, Start and Ends dates etc.)
   - The course tutors as avatars with email, name etc
       - If you are an root user:
           - Option to search for all enrolled users and change the enrollment (tutor/student)
   - Group information:
       - If you are a student and not distributed to a group:
           - The exercise groups with dates and tutors
           - Here you can set your preferences
       - If you are a student and distributed to a group:
           - Your group
           - Maybe an inline forum view or a link to the group forum
       - If you are a tutor/supertutor:
           - Your group with date and times
           - A link to send emails (inline?)
           - Maybe an inline forum view or a link to the group forum
       - If you are an root:
           - Options to create/edit/delete groups
           - Options to view all users in a group and an option to change a user from one group to another
   - Exercise sheets (download and link to the sheet for uploading/grading)
       - If you are an root:
           - Options to create/edit/delete sheets -> Extra view
   - Other course materials (Slides and Supplementary) (downloadable)
       - If you are an root:
           - Options to create/edit/delete materials (Inline?)
   - Statistics for Tutors and root
-}


module Pages.CourseDetail exposing (Model, Msg(..), init, update, view)

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
import Time
import Utils.Styles as Styles
import Api.Data.Course as Course
import Markdown as MD
import Utils.DateFormatter as DF


type Msg
    = NavigateTo Route


type alias Model =
    { course : WebData Course
    }


init : Int -> ( Model, Cmd Msg )
init id =
    ( { course = 
        RemoteData.Success
            { id = 0
            , name = "Informatik I"
            , description =
                Just """
# Lorem Ipsum!

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim 
veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea 
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate 
velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint 
occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
mollit anim id est laborum.

- info
- stuff

## Lots to learn

bla
"""
            , begins_at = Time.millisToPosix 1549888135000
            , ends_at = Time.millisToPosix 1560256135000
            , required_percentage = Just 250
            , sheets = Nothing
            , materials = Nothing
            } 
    }
    , Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    case model.course of 
        RemoteData.Success course ->
            div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0_ns, TC.w_100 ] ]
                [ div [ classes [ TC.w_75_l, TC.w_100, TC.ph5, TC.ph0_l, TC.center, TC.mw9_ns] ]
                    [ article [ classes [ TC.cf, TC.ph3, TC.ph5_ns, TC.pv4] ]
                        [ header [ classes [  TC.fn, TC.fl_ns, TC.w_50_ns, TC.pr4_ns ] ]
                            [ h1 [ classes [ TC.mb3, TC.mt0, TC.lh_title ] ] [ text course.name ]
                            , dl [ Styles.dateStyle ]
                                [ dt [ classes [ TC.black, TC.fw6 ] ] [ text "Beginn " ]
                                , dd [ classes [ TC.ml0 ] ] [ DF.fullDateFormatter sharedState course.begins_at ]
                                , dt [ classes [ TC.black, TC.fw6 ] ] [ text " Ende " ]
                                , dd [ classes [ TC.ml0 ] ] [ DF.fullDateFormatter sharedState course.ends_at ]
                                ]
                            ] 
                        , div 
                            [ classes 
                                [ TC.fn
                                , TC.fl_ns
                                , TC.w_50_ns
                                , TC.lh_copy
                                , TC.measure
                                , TC.mt4
                                , TC.mt0_ns
                                ]
                            ] 
                            [ MD.toHtml [ Styles.textStyle ] <| Maybe.withDefault "" course.description
                            ]
                        ]

                    ]
                ]

        _ ->
            div [] [] -- TODO loading, error etc.
