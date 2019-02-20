{-
   This is the course site. Here, all courses are listed.
       - New courses can be created/edited/deleted by root users.
       - Users can enroll in a course
       - Users can disenroll from the course
       - Courses are split between current and past (archive)
-}


module Pages.Courses exposing (Model, Msg(..), init, update, view, viewCoursesHeader, viewRenderCourse)

import Api.Data.AccountEnrollment exposing (AccountEnrollment)
import Api.Data.Course exposing (Course)
import Api.Data.CourseRole exposing (CourseRole(..))
import Api.Request.Courses as CoursesRequests
import Api.Request.Account as AccountRequests
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import Markdown as MD
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Utils.DateFormatter as DF
import Utils.Styles as Styles


type alias Model =
    { courseRequest : WebData (List Course)
    , accountEnrollmentsRequest : WebData (List AccountEnrollment)
    , enrollProgress : WebData String
    , disenrollProgress : WebData String
    , showArchive : Bool
    }


type Msg
    = CoursesResponse (WebData (List Course))
    | AccountEnrollmentsResponse (WebData (List AccountEnrollment))
    | Enroll Course
    | Disenroll Course
    | EnrollResponse (WebData String)
    | DisenrollResponse (WebData String)
    | ToggleArchive
    | NavigateTo Route


init : ( Model, Cmd Msg )
init =
    ( { courseRequest = Loading
      , accountEnrollmentsRequest = Loading
      , enrollProgress = NotAsked
      , disenrollProgress = NotAsked
      , showArchive = False
      }
    , Cmd.batch 
        [ AccountRequests.accountEnrollmentGet AccountEnrollmentsResponse
        , CoursesRequests.coursesGet CoursesResponse
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        CoursesResponse response ->
            ( { model | courseRequest = response }, Cmd.none, NoUpdate )

        AccountEnrollmentsResponse response ->
            ( { model | accountEnrollmentsRequest = response }, Cmd.none, NoUpdate )

        Enroll course ->
            ( model, Cmd.none, NoUpdate )

        Disenroll course ->
            ( model, Cmd.none, NoUpdate )

        EnrollResponse response ->
            ( model, Cmd.none, NoUpdate )

        DisenrollResponse response ->
            ( model, Cmd.none, NoUpdate )

        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        ToggleArchive ->
            ( { model | showArchive = not model.showArchive }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        translate =
            I18n.get sharedState.translations
    in
    case ( model.courseRequest, model.accountEnrollmentsRequest ) of
        ( Success courses, Success enrollments ) ->
            let
                currentTime =
                    Maybe.withDefault
                        (Time.millisToPosix 0)
                        -- THIS SHOULD NEVER HAPPEN!
                        sharedState.currentTime

                currentCourses =
                    courses
                        |> List.filter
                            (\course ->
                                Time.posixToMillis course.ends_at
                                    |> (<) (Time.posixToMillis currentTime)
                            )
                        |> List.map
                            (\course ->
                                viewRenderCourse sharedState course <|
                                    findEnrollmentForCourse course enrollments
                            )

                oldCourses =
                    courses
                        |> List.filter
                            (\course ->
                                Time.posixToMillis course.ends_at
                                    |> (>) (Time.posixToMillis currentTime)
                            )
                        |> List.map
                            (\course ->
                                viewRenderCourse sharedState course <|
                                    findEnrollmentForCourse course enrollments
                            )

                displayCourseOrNot =
                    if model.showArchive then
                        div
                            [ classes
                                [ TC.flex
                                , TC.flex_wrap
                                , TC.flex_row
                                , TC.justify_start
                                , TC.content_start
                                ]
                            ]
                            oldCourses

                    else
                        text ""

                userRole =
                    Maybe.withDefault { root = False } sharedState.role

                cTemp =
                    [ viewCoursesHeader "Aktuell" False userRole.root model
                    , div
                        [ classes
                            [ TC.cf
                            ]
                        ]
                        currentCourses
                    ]

                content =
                    if List.length oldCourses > 0 then
                        cTemp
                            ++ [ viewCoursesHeader "Archiv" True False model
                               , displayCourseOrNot
                               ]

                    else
                        cTemp
            in
            div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0_ns, TC.w_100 ] ]
                [ div
                    [ classes
                        [ TC.w_75_l
                        , TC.w_100
                        , TC.ph5
                        , TC.ph0_l
                        , TC.center
                        , TC.mw9_ns
                        ]
                    ]
                    content
                ]

        ( _, _ ) ->
            div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.w_100 ] ] []


viewCoursesHeader : String -> Bool -> Bool -> Model -> Html Msg
viewCoursesHeader lbl toggable creatable model =
    let
        toggleText =
            if model.showArchive then
                text "Hide"

            else
                text "Show"

        toggle =
            if toggable then
                button
                    [ Styles.buttonGreyStyle
                    , classes [ TC.br_pill, TC.ph3, TC.pv3 ]
                    , onClick ToggleArchive
                    ]
                    [ toggleText ]

            else
                text ""

        create =
            if creatable then
                button
                    [ Styles.buttonGreenStyle
                    , classes [ TC.br_pill, TC.ph4, TC.pv3 ]
                    ]
                    [ text "+" ]

            else
                text ""
    in
    div
        [ classes
            [ TC.w_100
            , TC.flex
            , TC.flex_row
            , TC.justify_between
            , TC.items_center
            , TC.bb
            , TC.bw2
            ]
        ]
        [ h1 [ Styles.headerStyle ] [ text lbl ]
        , toggle
        , create
        ]


viewRenderCourse : SharedState -> Course -> Maybe AccountEnrollment -> Html Msg
viewRenderCourse sharedState course enrollment =
    let
        ( buttonText, buttonMsg ) =
            case enrollment of
                Nothing ->
                    ( "Enroll", Enroll course )

                Just _ ->
                    ( "Show", NavigateTo <| CourseDetailRoute course.id )
    in
    article [ classes [ TC.cf, TC.fl, TC.ph3, TC.pv5, TC.w_100, TC.w_50_m, TC.w_third_ns ] ]
        [ header [ classes [ TC.measure ] ]
            [ h1 [ Styles.listHeadingStyle ] [ text course.name ] -- Bold header
            , dl [ Styles.dateStyle ]
                [ dt [ classes [ TC.black, TC.fw6 ] ] [ text "Beginn " ]
                , dd [ classes [ TC.ml0 ] ] [ DF.fullDateFormatter sharedState course.begins_at ]
                , dt [ classes [ TC.black, TC.fw6 ] ] [ text " Ende " ]
                , dd [ classes [ TC.ml0 ] ] [ DF.fullDateFormatter sharedState course.ends_at ]
                ]
            ]
        , div [ classes [ TC.measure ] ]
            [ MD.toHtml [ Styles.textStyle ] <| Maybe.withDefault "" course.description -- Normal paragraph
            , button
                [ Styles.buttonGreyStyle
                , classes [ TC.w_100 ]
                , onClick buttonMsg
                ]
                [ text buttonText ]

            -- TODO check if user is enrolled or not. Either show and execute enroll or disenroll
            ]
        ]


findEnrollmentForCourse : Course -> List AccountEnrollment -> Maybe AccountEnrollment
findEnrollmentForCourse course enrollments =
    enrollments
        |> List.filter (\enrollment -> enrollment.course_id == course.id)
        |> List.head
