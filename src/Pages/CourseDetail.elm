{-
   This is the course detail page. It displays:
   - Basic information (Title, Description, Start and Ends dates etc.)
   - The course tutors as avatars with email, name etc
       - If you are a root user:
           - Option to search for all enrolled users and change the enrollment (tutor/student) 
           
           (done?)
   - Group information:
       - If you are a student and not distributed to a group:
           - The exercise groups with dates and tutors
           - Here you can set your preferences
       - If you are a student and distributed to a group:
           - Your group
       - If you are a tutor/supertutor:
           - Your group with date and times
           - A link to send emails (inline?)
       - If you are a root:
           - Options to create/edit/delete groups
           - Options to view all users in a group and an option to change a user from one group to another
   - Exercise sheets (download and link to the sheet for uploading/grading)
       - If you are a root:
           - Options to create/edit/delete sheets -> Extra view
   - Other course materials (Slides and Supplementary) (downloadable)
       - If you are a root:
           - Options to create/edit/delete materials (Inline?)
   - Statistics for Tutors and root
-}


module Pages.CourseDetail exposing (Model, Msg(..), init, update, view)

import Api.Data.AccountEnrollment as AccountEnrollment exposing (AccountEnrollment)
import Api.Data.Course exposing (Course)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupBid as GroupBid exposing (GroupBid)
import Api.Data.GroupEnrollmentChange as GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Data.User as User exposing (User)
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Request.Account as AccountRequests
import Api.Request.Courses as CoursesRequests
import Api.Request.Groups as GroupsRequests
import Browser.Navigation exposing (pushUrl)
import Components.Dropdown as Dropdown exposing (ToggleEvent(..), drawer, dropdown, toggle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Events.Extra exposing (onEnter)
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


type Msg
    = NavigateTo Route
    | CourseResponse (WebData Course) -- Get basic information about the course
    | CourseRoleResponse (WebData (List AccountEnrollment)) -- Used to determine the course role
    | EnrollmentsResponse (WebData (List UserEnrollment)) -- List all enrollments in the course. Only used for students and tutors
    | SearchUserForEnrollmentResponse (WebData (List UserEnrollment)) -- Search for a specific user to change the enrollment
    | EnrollmentChangedResponse (WebData ()) -- Set the enrollment state for the searched user -- TODO set correct return
    | GroupsListResponse (WebData (List Group)) -- List all groups. Only visible for unenrolled students/tutors and admins
    | GroupDisplayResponse (WebData Group) -- Show the assigned group for students. For tutors per default their group (can be changed). Not visible for admins
    | GroupBidResponse (WebData GroupBid) -- Response for a group bid (only students)
    | SearchUserForGroupResponse (WebData UserEnrollment) -- Search for a specific user to change the group (Only admins)
    | GroupChangedResponse (WebData GroupEnrollmentChange) -- Response for a group change initiated by an admin
    | SetField Field String
    | SearchUserForEnrollment
    | SearchUserForGroup
    | ChangeEnrollment UserEnrollment
    | ChangeGroup Int GroupEnrollmentChange
    | ToggleRoleDropdown Bool


type alias Model =
    { courseId : Int
    , courseRole : Maybe CourseRole
    , courseRequest : WebData Course
    , courseRoleRequest : WebData (List AccountEnrollment)
    , enrollmentsRequest : WebData (List UserEnrollment)
    , searchUserForEnrollmentRequest : WebData (List UserEnrollment)
    , enrollmentChangedRequest : WebData ()
    , groupsRequest : WebData (List Group)
    , groupRequest : WebData Group
    , groupBidRequest : WebData GroupBid
    , searchUserForGroupRequest : WebData UserEnrollment
    , groupChangedRequest : WebData GroupEnrollmentChange
    , searchEnrollmentInput : String
    , searchGroupInput : String
    , roleDropdown : Dropdown.State
    , groupDropdown : Dropdown.State
    }


init : Int -> ( Model, Cmd Msg )
init id =
    ( { courseId = id
      , courseRole = Nothing
      , courseRequest = Loading
      , courseRoleRequest = Loading
      , enrollmentsRequest = NotAsked
      , searchUserForEnrollmentRequest = NotAsked
      , enrollmentChangedRequest = NotAsked
      , groupsRequest = NotAsked
      , groupRequest = NotAsked
      , groupBidRequest = NotAsked
      , searchUserForGroupRequest = NotAsked
      , groupChangedRequest = NotAsked
      , searchEnrollmentInput = ""
      , searchGroupInput = ""
      , roleDropdown = False
      , groupDropdown = False
      }
    , Cmd.batch
        [ AccountRequests.accountEnrollmentGet CourseRoleResponse
        , CoursesRequests.courseGet id CourseResponse
        ]
    )


determineInitialRoleRequests : Model -> CourseRole -> ( Model, Cmd Msg )
determineInitialRoleRequests model role =
    case role of
        Admin ->
            ( model
            , Cmd.none
            )

        Tutor ->
            ( { model
                | enrollmentsRequest = Loading
                , groupRequest = Loading
              }
            , Cmd.batch
                [ CoursesRequests.coursesEnrollmentGetTeam model.courseId EnrollmentsResponse
                , CoursesRequests.courseOwnGroupGet model.courseId GroupDisplayResponse
                ]
            )

        Student ->
            ( { model
                | enrollmentsRequest = Loading
                , groupRequest = Loading
              }
            , Cmd.batch
                [ CoursesRequests.coursesEnrollmentGetTeam model.courseId EnrollmentsResponse
                , CoursesRequests.courseOwnGroupGet model.courseId GroupDisplayResponse
                ]
            )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        CourseResponse response ->
            ( { model | courseRequest = response }, Cmd.none, NoUpdate )

        CourseRoleResponse (Success roles) ->
            case determineRole model.courseId roles of
                Just role ->
                    -- We received the users course role.
                    let
                        -- Determine what commands need to be send depending on the role
                        updatedModel =
                            { model | courseRole = Just role, courseRoleRequest = Success roles }

                        ( newModel, newCmds ) =
                            determineInitialRoleRequests updatedModel role
                    in
                    ( newModel, newCmds, NoUpdate )

                -- Execute the changes
                Nothing ->
                    -- Whoops. The user is not enrolled in the course. Navigate back to the courses page
                    ( model, pushUrl sharedState.navKey (reverseRoute CoursesRoute), NoUpdate )

        SearchUserForEnrollment ->
            ( { model | searchUserForEnrollmentRequest = Loading }, CoursesRequests.coursesEnrollmentGetByEmail model.courseId model.searchEnrollmentInput SearchUserForEnrollmentResponse, NoUpdate )

        SearchUserForEnrollmentResponse response ->
            ( { model | searchUserForEnrollmentRequest = response }, Cmd.none, NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        _ ->
            ( model, Cmd.none, NoUpdate )



-- TODO if successful update searchProgress too


view : SharedState -> Model -> Html Msg
view sharedState model =
    case ( model.courseRoleRequest, model.courseRole ) of
        ( Success _, Just role ) ->
            div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0, TC.w_100 ] ]
                [ div [ classes [ TC.w_75_l, TC.w_100, TC.ph0_l, TC.ph3_m, TC.ph2, TC.center, TC.mw9_ns ] ] <|
                    viewCourseInfo sharedState model
                        ++ viewDetermineTeamOrSearch role sharedState model
                ]

        ( _, _ ) ->
            div [] []


viewCourseInfo : SharedState -> Model -> List (Html Msg)
viewCourseInfo sharedState model =
    case model.courseRequest of
        RemoteData.Success course ->
            [ article [ classes [ TC.cf, TC.ph3, TC.ph5_ns, TC.pt4 ] ]
                [ header [ classes [ TC.fn, TC.fl_ns, TC.w_50_ns, TC.pr4_ns ] ]
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
            [ text "" ]



-- TODO loading, error etc.


viewDetermineTeamOrSearch : CourseRole -> SharedState -> Model -> List (Html Msg)
viewDetermineTeamOrSearch courseRole sharedState model =
    case courseRole of
        Admin ->
            viewMemberSearch sharedState model

        _ ->
            viewTeam sharedState model


viewTeam : SharedState -> Model -> List (Html Msg)
viewTeam sharedState model =
    case model.enrollmentsRequest of
        RemoteData.Success enrollments ->
            let
                sortedTeam =
                    List.sortWith compareRoleName enrollments
            in
            [ div [ classes [ TC.ph3, TC.ph5_ns ] ]
                [ h1 [ Styles.headerStyle, classes [ TC.w_100, TC.bt, TC.bw2, TC.pt5_ns, TC.pt4, TC.mb4_ns, TC.mb3 ] ] [ text "Team" ]
                , div [ classes [ TC.flex, TC.flex_row, TC.flex_wrap, TC.justify_between ] ] <|
                    List.map viewTeamMember sortedTeam
                ]
            ]

        _ ->
            [ text "Loading" ]


viewTeamMember : UserEnrollment -> Html Msg
viewTeamMember userEnrollment =
    let
        user =
            userEnrollment.user

        avatar =
            case user.avatarUrl of
                Just avatarUrl ->
                    avatarUrl

                Nothing ->
                    "assets/defaultAvatar.png"
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
            ]
            []
        , div [ classes [ TC.flex_auto, TC.pl3 ] ]
            [ h1 [ Styles.listHeadingStyle, classes [ TC.mv0 ] ] [ text (user.firstname ++ " " ++ user.lastname) ]
            , h2 [ Styles.textStyle, classes [ TC.mv0 ] ] [ text user.email ] -- TODO make clickable
            ]
        ]


viewMemberSearch : SharedState -> Model -> List (Html Msg)
viewMemberSearch sharedState model =
    let
        displaySearchResults =
            case model.searchUserForEnrollmentRequest of
                Success userEnrollment ->
                    viewUserSearchResult model <| List.head userEnrollment

                _ ->
                    text ""
    in
    [ div [ classes [ TC.ph3, TC.ph5_ns ] ]
        [ h1
            [ Styles.headerStyle
            , classes [ TC.w_100, TC.bt, TC.bw2, TC.pt5_ns, TC.pt4, TC.mb4_ns, TC.mb3 ]
            ]
            [ text "Change role" ]
        , div [ classes [ TC.w_100, TC.h3, TC.v_mid, TC.flex, TC.items_center ] ]
            [ input
                [ Styles.lineInputStyle
                , type_ "email"
                , placeholder "E-Mail"
                , onInput <| SetField EnrollmentSearchField
                , onEnter SearchUserForEnrollment
                , classes [ TC.measure, TC.w_90 ]
                ]
                []
            , input
                [ type_ "image"
                , src "assets/magnify.svg"
                , classes [ TC.ml2, TC.w2, TC.h2, TC.pa1, TC.dim ]
                , onClick SearchUserForEnrollment
                ]
                []
            ]
        ]
    , displaySearchResults
    ]


viewUserSearchResult : Model -> Maybe UserEnrollment -> Html Msg
viewUserSearchResult model maybeUserEnrollment =
    case maybeUserEnrollment of
        Just userEnrollment ->
            (let
                user =
                    userEnrollment.user

                avatar =
                    case user.avatarUrl of
                        Just avatarUrl ->
                            avatarUrl

                        Nothing ->
                            "assets/defaultAvatar.png"

                currentRoleString =
                    case userEnrollment.role of
                        Student ->
                            "Student"

                        Tutor ->
                            "Tutor"

                        Admin ->
                            "Admin"
            in
            div [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph3, TC.ph5_ns ] ]
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
                    ]
                    []
                , div [ classes [ TC.ph3 ] ]
                    [ h1 [ Styles.listHeadingStyle, classes [ TC.mv0 ] ] [ text (user.firstname ++ " " ++ user.lastname) ]
                    , h2 [ Styles.textStyle, classes [ TC.mv0 ] ] [ text user.email ] -- TODO make clickable
                    ]
                , div [ classes [ TC.ml4, TC.w4 ] ]
                    [ dropdown
                        div
                        []
                        [ toggle
                            button
                            [ classes [ TC.w4, TC.tc, TC.button_reset, TC.bg_white ]
                            , Styles.lineInputStyle
                            , Styles.labelStyle
                            ]
                            [ text currentRoleString ]
                        , drawer div
                            []
                        <|
                            List.map
                                (\( role, label ) ->
                                    button
                                        [ onClick <| ChangeEnrollment { userEnrollment | role = role }
                                        , classes
                                            [ TC.w_100
                                            , TC.bl_0
                                            , TC.br_0
                                            , TC.bb_0
                                            , TC.bt
                                            , TC.b__black_20
                                            , TC.tc
                                            , TC.button_reset
                                            , TC.bg_near_white
                                            , TC.ph2
                                            , TC.pv3
                                            , TC.grow
                                            , TC.pointer
                                            , TC.shadow_5
                                            ]
                                        , Styles.textStyle
                                        ]
                                        [ text label ]
                                )
                                [ ( Student, "Student" )
                                , ( Tutor, "Tutor" )
                                , ( Admin, "Admin" )
                                ]
                        ]
                        model.roleDropdown
                        roleDropdownConfig
                    ]
                ])

        Nothing ->
            h2 
                [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph3, TC.ph5_ns ]
                , Styles.listHeadingStyle ] [text "Not found"]


viewDetermineGroupDisplay : CourseRole -> SharedState -> Model -> List (Html Msg)
viewDetermineGroupDisplay courseRole sharedState model =
    [ div [] [] -- Check Role

    -- If Admin -> Show all groups. Option to create, edit, delete, email groups & search for students to change group
    -- If Tutor -> Option to change group. Show own group (with members), option to email own group. Show date and times
    -- If Student -> Check if own group is set
    -- Not set: Display bidding screen
    -- If set: Display own group. With members. Option to send email to other members and tutor
    ]


compareRoleName : UserEnrollment -> UserEnrollment -> Order
compareRoleName userA userB =
    case ( userA.role, userB.role ) of
        ( Admin, Admin ) ->
            compare userA.user.lastname userB.user.lastname

        ( Admin, Tutor ) ->
            LT

        ( Tutor, Admin ) ->
            GT

        ( Tutor, Tutor ) ->
            compare userA.user.lastname userB.user.lastname

        ( Tutor, _ ) ->
            LT

        ( _, Tutor ) ->
            GT

        ( _, _ ) ->
            compare userA.user.lastname userB.user.lastname


determineRole : Int -> List AccountEnrollment -> Maybe CourseRole
determineRole course_id enrollments =
    List.head <|
        List.map .role <|
            List.filter (\enrollment -> enrollment.course_id == course_id) enrollments


roleDropdownConfig : Dropdown.Config Msg
roleDropdownConfig =
    Dropdown.Config
        "roleDropdown"
        OnClick
        (class "visible")
        ToggleRoleDropdown


type Field
    = EnrollmentSearchField
    | GroupSearchField


setField : Model -> Field -> String -> Model
setField model field value = 
    case field of
        EnrollmentSearchField ->
            { model | searchEnrollmentInput = value }

        GroupSearchField ->
            { model | searchGroupInput = value }