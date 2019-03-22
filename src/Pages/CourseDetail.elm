{-
   This is the course detail page. It displays:
   - Basic information (Title, Description, Start and Ends dates etc.)
   - The course tutors as avatars with email, name etc
       - If you are a root user:
           - Option to search for all enrolled users and change the enrollment (tutor/student)

           (done)
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
import Api.Data.Sheet as Sheet exposing (Sheet)
import Api.Data.User as User exposing (User)
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Request.Account as AccountRequests
import Api.Request.Courses as CoursesRequests
import Api.Request.Groups as GroupsRequests
import Api.Endpoint exposing (unwrap, sheetFile)
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements exposing 
    ( inputElement
    , multiButton
    , rRowHeaderActionButtons
    , pageContainer
    , normalPage
    , widePage
    , rContainer
    , rRow
    , rRowExtraSpacing
    , rRowHeader
    , r2Column
    , datesDisplayContainer
    , dateElement
    )
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
import Utils.Utils exposing (perform, flip, handleLogoutErrors, tupleMapThree)
import File.Download as Download
import Components.UserAvatarEmailView as UserView
import Components.Groups.BiddingView as BiddingView


type Msg
    = NavigateTo Route
    | CourseResponse (WebData Course) -- Get basic information about the course
    | CourseRoleResponse (WebData (List AccountEnrollment)) -- Used to determine the course role
    | EnrollmentsResponse (WebData (List UserEnrollment)) -- List all enrollments in the course. Only used for students and tutors
    | SearchUserForEnrollmentResponse (WebData (List UserEnrollment)) -- Search for a specific user to change the enrollment
    | EnrollmentChangedResponse (WebData ()) -- Set the enrollment state for the searched user -- TODO set correct return
    | GroupDisplayResponse (WebData Group) -- Show the assigned group for students. For tutors per default their group (can be changed). Not visible for admins
    | SearchUserForGroupResponse (WebData UserEnrollment) -- Search for a specific user to change the group (Only admins)
    | GroupChangedResponse (WebData GroupEnrollmentChange) -- Response for a group change initiated by an admin
    | GroupMsg GroupMsgTypes
    | SheetRequestResponse (WebData (List Sheet))
    | SetField Field String
    | SearchUserForEnrollment
    | SearchUserForGroup
    | ChangeEnrollment UserEnrollment
    | ChangeGroup Int GroupEnrollmentChange
    | WriteTo Int
    | DownloadSheet Int Int


type alias Model =
    { courseId : Int
    , courseRole : Maybe CourseRole
    , courseRequest : WebData Course
    , sheetRequest : WebData (List Sheet)
    , courseRoleRequest : WebData (List AccountEnrollment)
    , enrollmentsRequest : WebData (List UserEnrollment)
    , searchUserForEnrollmentRequest : WebData (List UserEnrollment)
    , enrollmentChangedRequest : WebData ()
    , groupRequest : WebData Group
    , groupModel : Maybe GroupModel
    , searchUserForGroupRequest : WebData UserEnrollment
    , searchEnrollmentInput : String
    , searchGroupInput : String
    }

type GroupMsgTypes
    = BiddingMsg BiddingView.Msg

type GroupModel
    = BiddingModel BiddingView.Model


init : Int -> ( Model, Cmd Msg )
init id =
    ( { courseId = id
      , courseRole = Nothing
      , courseRequest = Loading
      , sheetRequest = Loading
      , courseRoleRequest = Loading
      , enrollmentsRequest = NotAsked
      , searchUserForEnrollmentRequest = NotAsked
      , enrollmentChangedRequest = NotAsked
      , groupRequest = NotAsked
      , groupModel = Nothing
      , searchUserForGroupRequest = NotAsked
      , searchEnrollmentInput = ""
      , searchGroupInput = ""
      }
    , Cmd.batch
        [ AccountRequests.accountEnrollmentGet CourseRoleResponse
        , CoursesRequests.courseGet id CourseResponse
        , CoursesRequests.courseSheetsGet id SheetRequestResponse
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

        EnrollmentsResponse response ->
            ( { model | enrollmentsRequest = response}, Cmd.none, NoUpdate)

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        ChangeEnrollment enrollment ->
            ( model
            , CoursesRequests.coursesEnrollmentsUserPut model.courseId 
                enrollment.user.id enrollment.role EnrollmentChangedResponse
            , NoUpdate
            )

        EnrollmentChangedResponse (Success _) ->
            ( model, perform SearchUserForEnrollment, NoUpdate )

        SheetRequestResponse response ->
            ( {model | sheetRequest = response}, Cmd.none, NoUpdate )

        DownloadSheet courseId sheetId ->
            ( model, Download.url <| unwrap <| sheetFile courseId sheetId, NoUpdate)

        WriteTo userId ->
            ( model, UserView.updateFromUserAvatar sharedState userId, NoUpdate)

        GroupDisplayResponse response ->
            updateOwnGroupDisplay sharedState model response

        GroupMsg subMsg ->
            let
                (subModel, subCmd, subSharedState) = 
                    case (model.groupModel, subMsg) of
                        (Just (BiddingModel biddingModel), BiddingMsg bidMsg) ->
                            tupleMapThree
                                (\m -> Just <| BiddingModel m)
                                (\m -> Cmd.map GroupMsg <| Cmd.map BiddingMsg m) 
                                (\v -> v) <|
                            BiddingView.update sharedState bidMsg biddingModel

                        (_, _) -> (Nothing, Cmd.none, NoUpdate)
            in
            ( { model | groupModel = subModel }
            , subCmd
            , subSharedState
            )

        _ ->
            ( model, Cmd.none, NoUpdate )


updateOwnGroupDisplay : SharedState -> Model -> WebData Group -> (Model, Cmd Msg, SharedStateUpdate)
updateOwnGroupDisplay sharedState model response =
    case response of
        Failure err ->
            handleLogoutErrors {model | groupRequest = response } sharedState 
                (\e ->
                    let
                        maybeInit = case e of
                            Http.BadStatus 404 ->
                                Just <| 
                                    Tuple.mapBoth 
                                        (\subModel -> BiddingModel subModel)
                                        (\subMsg -> Cmd.map BiddingMsg subMsg) <|
                                    BiddingView.init model.courseId
                            _ ->
                                Nothing
                    in
                    ( { model 
                        | groupRequest = response 
                        , groupModel = Maybe.map Tuple.first maybeInit
                    }
                    , Maybe.withDefault Cmd.none <| 
                        Maybe.map (\subInit ->
                            Cmd.map GroupMsg <| Tuple.second subInit) maybeInit
                    , NoUpdate
                    )              
                ) 
                err

        _ ->
            ( { model | groupRequest = response }, Cmd.none, NoUpdate)

-- TODO if successful update searchProgress too


view : SharedState -> Model -> Html Msg
view sharedState model =
    case ( model.courseRoleRequest, model.courseRole ) of
        ( Success _, Just role ) ->
            pageContainer <|
                [ widePage <| [ viewCourseInfo sharedState model ]
                , normalPage <|
                    [ viewDetermineTeamOrSearch role sharedState model
                    , viewDetermineGroupDisplay role sharedState model
                    , viewSheets sharedState model
                    ]
                ]

        ( _, _ ) ->
            div [] []


viewCourseInfo : SharedState -> Model -> Html Msg
viewCourseInfo sharedState model =
    case model.courseRequest of
        RemoteData.Success course ->
            div [ classes [TC.w_100, TC.b__black, TC.bw2, TC.bb, TC.pb3, TC.overflow_hidden] ]
                ( r2Column 
                    [ div []
                        [ h1 [ classes [ TC.mb3, TC.mt0, TC.lh_title ] ] [ text course.name ]
                        , datesDisplayContainer <|
                            (dateElement "Beginn " <| DF.fullDateFormatter sharedState course.begins_at) ++
                            (dateElement "Ende " <| DF.fullDateFormatter sharedState course.ends_at)
                        ]
                    ]
                    [ MD.toHtml [ Styles.textStyle ] <| course.description ]
                )

        _ ->
            text ""



-- TODO loading, error etc.


viewDetermineTeamOrSearch : CourseRole -> SharedState -> Model -> Html Msg
viewDetermineTeamOrSearch courseRole sharedState model =
    case courseRole of
        Admin ->
            viewMemberSearch sharedState model

        _ ->
            viewTeam sharedState model


viewTeam : SharedState -> Model -> Html Msg
viewTeam sharedState model =
    case model.enrollmentsRequest of
        RemoteData.Success enrollments ->
            let
                sortedTeam =
                    List.sortWith compareRoleName enrollments
            in
            rContainer <|
                [ rRowHeader "Team"
                , div [ classes [ TC.flex, TC.flex_row, TC.flex_wrap, TC.justify_start ] ]
                    ( sortedTeam |>
                        List.map (\ue -> UserView.initFromUser ue.user) |>
                            List.map Tuple.first |>
                                List.map (\uv -> UserView.view sharedState uv WriteTo)
                    )
                ]

        _ ->
            text "Loading"


viewMemberSearch : SharedState -> Model -> Html Msg
viewMemberSearch sharedState model =
    let
        displaySearchResults =
            case model.searchUserForEnrollmentRequest of
                Success userEnrollment ->
                    viewUserSearchResult model <| List.head userEnrollment

                _ ->
                    text ""
    in
    div [ classes [ TC.ph3, TC.ph5_ns ] ]
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
        , displaySearchResults
        ]



viewUserSearchResult : Model -> Maybe UserEnrollment -> Html Msg
viewUserSearchResult model maybeUserEnrollment =
    case maybeUserEnrollment of
        Just userEnrollment ->
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
            div [ classes [ TC.flex, TC.flex_wrap, TC.items_center, TC.pa3, TC.ph3, TC.ph5_ns ] ]
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
                , div [ classes [ TC.ml4, TC.flex ] ]
                    [ multiButton
                        [ ("Student", userEnrollment.role == Student, ChangeEnrollment {userEnrollment | role = Student} )
                        , ("Tutor", userEnrollment.role == Tutor, ChangeEnrollment {userEnrollment | role = Tutor} )
                        , ("Admin", userEnrollment.role == Admin, ChangeEnrollment {userEnrollment | role = Admin} )
                        ]
                    
                    ]
                ]

        Nothing ->
            h2
                [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph3, TC.ph5_ns ]
                , Styles.listHeadingStyle
                ]
                [ text "Not found" ]


viewDetermineGroupDisplay : CourseRole -> SharedState -> Model -> Html Msg
viewDetermineGroupDisplay courseRole sharedState model =
    {- Check Role

    If Admin -> Show all groups. Option to create, edit, delete, email groups & search for students to change group
    If Tutor -> Option to change group. Show own group (with members), option to email own group. Show date and times
    If Student -> Check if own group is set
        - Not set: Display bidding screen
        - If set: Display own group. With members. Option to send email to other members and tutor
    -}
    case model.groupModel of
        Just (BiddingModel biddingModel) ->
            rContainer <|
                [ rRowHeader "Gruppen Präferenzen"
                , BiddingView.view sharedState biddingModel
                    |> Html.map BiddingMsg
                    |> Html.map GroupMsg
                ]

        Nothing ->
            text ""


viewSheets : SharedState -> Model -> Html Msg
viewSheets sharedState model =
    rContainer <|
        [ rRowHeader "Sheets"
        , div [classes [TC.ph4] ] <|
            case model.sheetRequest of
                Success sheets ->
                    sheets |>
                        List.sortBy (\sheet -> Time.posixToMillis sheet.due_at ) |>
                        List.map (\sheet ->
                            rRowHeaderActionButtons sheet.name Styles.listHeadingStyle <|
                                ([
                                    ( "Download"
                                    , DownloadSheet model.courseId sheet.id
                                    , Styles.buttonGreyStyle)
                                ,
                                    ( "Show"
                                    , NavigateTo <| SheetDetailRoute model.courseId sheet.id
                                    , Styles.buttonGreyStyle)
                                ] ++ 
                                    ( if model.courseRole == Just Admin then
                                        [
                                            ( "Edit"
                                            , NavigateTo <| EditSheetRoute model.courseId sheet.id
                                            , Styles.buttonGreyStyle)
                                        
                                        ]
                                    else
                                        []
                                    )
                                )
                        ) |>
                        flip List.append
                            [ if model.courseRole == Just Admin then
                                rRowHeaderActionButtons "New sheet" Styles.listHeadingStyle 
                                    [ 
                                        ( "Create"
                                        , NavigateTo <| CreateSheetRoute model.courseId
                                        , Styles.buttonGreenStyle) 
                                    ]
                                
                            else
                                text ""
                            ]
                        
                _ -> 
                    [ div [] [ text "Loading"] ]
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
