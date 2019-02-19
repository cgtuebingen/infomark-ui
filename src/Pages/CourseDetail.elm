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
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Data.User as User exposing (User)
import Api.Request.UserEnrollments as UserEnrollments
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
import Markdown as MD
import Utils.DateFormatter as DF


type Msg
    = NavigateTo Route
    | CourseResponse (WebData Course)
    | EnrollmentResponse (WebData (List UserEnrollment))


type alias Model =
    { courseProgress : WebData Course
    , enrollmentProgress : WebData (List UserEnrollment)
    }


init : Int -> ( Model, Cmd Msg )
init id =
    ( 
    { courseProgress = 
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
    , enrollmentProgress = RemoteData.Success
        [ { role = Admin
          , user = 
                { id = 0
                , firstname = "root"
                , lastname = "root"
                , avatarUrl = Nothing
                , email = "root@root.com"
                , studentNumber = Nothing
                , semester = Nothing
                , subject = Nothing
                , language = Just "en"
                }
          }
        , { role = Tutor
          , user =
                { id = 1
                , firstname = "Max"
                , lastname = "Mustermann"
                , avatarUrl = Nothing
                , email = "max.mustermann@uni-tuebingen.de"
                , studentNumber = Nothing
                , semester = Nothing
                , subject = Nothing
                , language = Just "de"
                }
          }
        , { role = Tutor
          , user =
                { id = 2
                , firstname = "Peter"
                , lastname = "Pan"
                , avatarUrl = Just "assets/Logo.png"
                , email = "peter.pan@student.uni-tuebingen.de"
                , studentNumber = Just "124567"
                , semester = Just 2
                , subject = Just "Informatik"
                , language = Just "de"
                }
          }
        ]
    
       
    }
    , Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )

        EnrollmentResponse response ->
            ( { model | enrollmentProgress = response }, Cmd.none, NoUpdate )

        CourseResponse response ->
            ( { model | courseProgress = response }, Cmd.none, NoUpdate )



view : SharedState -> Model -> Html Msg
view sharedState model =
    div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0, TC.w_100 ] ]
        [ div [ classes [ TC.w_75_l, TC.w_100, TC.ph0_l, TC.ph3_m, TC.ph2, TC.center, TC.mw9_ns] ]
            <| (viewCourseInfo sharedState model) ++ (viewTeam sharedState model)
        ]

    
viewCourseInfo : SharedState -> Model -> List (Html Msg)
viewCourseInfo sharedState model =
    case model.courseProgress of 
        RemoteData.Success course ->
            [ article [ classes [ TC.cf, TC.ph3, TC.ph5_ns, TC.pt4] ]
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
                        , TC.measure_wide
                        , TC.mt4
                        , TC.mt0_ns
                        ]
                    ] 
                    [ MD.toHtml [ Styles.textStyle ] <| Maybe.withDefault "" course.description
                    ]
                ]

            ]
                

        _ ->
            [ text "" ] -- TODO loading, error etc.


viewTeam : SharedState -> Model -> List (Html Msg)
viewTeam sharedState model =
    case model.enrollmentProgress of
        RemoteData.Success enrollments ->
            let 
                sortedTeam = List.sortWith compareRoleName enrollments
            in
            [ div [classes [TC.ph3, TC.ph5_ns]] 
                [ h1 [ Styles.headerStyle, classes [TC.w_100, TC.bt, TC.bw2, TC.pt5_ns, TC.pt4, TC.mb4_ns, TC.mb3] ] [ text "Team" ] 
                , div [ classes [TC.flex, TC.flex_row, TC.flex_wrap, TC.justify_between] ] <| 
                    List.map viewTeamMember sortedTeam 
                ]
            ]


        _ -> [ text "Loading" ]

viewTeamMember : UserEnrollment -> Html Msg
viewTeamMember userEnrollment =
    let
        user = userEnrollment.user
        avatar = case user.avatarUrl of
            Just avatarUrl -> avatarUrl
            Nothing -> "assets/defaultAvatar.png"
    in
    div [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph0_l ] ] 
        [ img 
            [ src avatar
            , classes 
                [ TC.br_100
                , TC.ba
                , TC.b__black_10
                , TC.shadow_5_ns
                , TC.h3_ns
                , TC.h2
                , TC.w3_ns
                , TC.w2
                ]
            ] []
        , div [ classes [TC.flex_auto, TC.pl3] ]
            [ h1 [ Styles.listHeadingStyle, classes [TC.mv0] ] [ text (user.firstname ++ " " ++ user.lastname) ]
            , h2 [ Styles.textStyle, classes [TC.mv0] ] [ text user.email ] -- TODO make clickable
            ]
        ]


getTeam : Course -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
getTeam course msg =
    UserEnrollments.courseEnrollmentGetTeam course.id msg


compareRoleName : UserEnrollment -> UserEnrollment -> Order
compareRoleName userA userB =
    case (userA.role, userB.role) of
        (Admin, Admin) -> compare userA.user.lastname userB.user.lastname
        (Admin, Tutor) -> LT
        (Tutor, Admin) -> GT
        (Tutor, Tutor) -> compare userA.user.lastname userB.user.lastname
        (Tutor, _) -> LT
        (_, Tutor) -> GT
        (_, _) -> compare userA.user.lastname userB.user.lastname
  