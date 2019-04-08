module Components.Groups.AdminView exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

{-| Group view for Admins:

  - Options to create/edit/delete groups
  - Options to view all users in a group
  - Option to reassign users to a different group

-}

import Api.Data.Group exposing (Group)
import Api.Data.GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Data.User exposing (User)
import Api.Data.UserEnrollment exposing (UserEnrollment)
import Api.Request.Courses as CourseRequests
import Api.Request.Groups as GroupRequests
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbState(..)
        , inputElement
        , multiButton
        , r1Column
        , r2Column
        , rContainer
        , rRow
        , rRowButton
        , rRowHeaderActionButtons
        )
import Components.UserAvatarEmailView as UserView
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra as ME
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors, split)


type Msg
    = GetGroupsResponse (WebData (List Group))
    | GetEnrollmentResponse Int (WebData (List UserEnrollment))
    | SendMailToGroup Int Int
    | SendMailToCourse Int
    | ToggleUsers Int
    | ReassignUser User Group Group
    | ReassignUserResponse (List Int) (WebData ())
    | EditGroup Int Int
    | CreateGroup Int
    | WriteEmailToUser Int --Currently not used



--| SendGroupResponse Int Int (WebData ())


type alias GroupDetail =
    { users : Maybe (List User)
    , tutor : Maybe User
    , group : Group
    }


type alias Model =
    { course_id : Int
    , groupsRequest : WebData (List Group)
    , enrollmentRequests : Dict Int (WebData (List UserEnrollment))
    , groups : Dict Int ( Group, List UserEnrollment )
    , userVisibleForGroup : Maybe Int
    }


init : Int -> List Group -> ( Model, Cmd Msg )
init courseId groups =
    let
        model =
            { course_id = courseId
            , groupsRequest = Success groups
            , enrollmentRequests = Dict.empty
            , groups = Dict.empty
            , userVisibleForGroup = Nothing
            }
    in
    ( model
    , performUserEnrollmentRequests model
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        GetGroupsResponse response ->
            ( fillGroupDictFromRequest { model | groupsRequest = response }
            , Cmd.none
            , NoUpdate
            )

        GetEnrollmentResponse gId response ->
            let
                newDict =
                    Dict.insert gId response model.enrollmentRequests
            in
            ( fillGroupDictFromRequest { model | enrollmentRequests = newDict }
            , Cmd.none
            , NoUpdate
            )

        SendMailToGroup courseId groupId ->
            ( model
            , pushUrl sharedState.navKey (reverseRoute <| MailToGroupRoute courseId groupId)
            , NoUpdate
            )

        SendMailToCourse courseId ->
            ( model
            , pushUrl sharedState.navKey (reverseRoute <| MailToCourseRoute courseId)
            , NoUpdate
            )

        ToggleUsers groupId ->
            let
                up =
                    if model.userVisibleForGroup == Just groupId then
                        -- If the same id is pressed twice hide the users
                        Nothing

                    else
                        -- Else show them
                        Just groupId
            in
            ( { model | userVisibleForGroup = up }, Cmd.none, NoUpdate )

        ReassignUser user oldGroup newGroup ->
            ( model, changeUsersGroup model user oldGroup newGroup, NoUpdate )

        ReassignUserResponse groupChangedIds response ->
            updateReasignUserResponse model groupChangedIds response

        EditGroup courseId groupId ->
            ( model, pushUrl sharedState.navKey (reverseRoute <| EditGroupRoute courseId groupId), NoUpdate )

        CreateGroup courseId ->
            ( model, pushUrl sharedState.navKey (reverseRoute <| CreateGroupRoute courseId), NoUpdate )

        WriteEmailToUser userId ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        allGroupsWithUsers =
            Dict.values model.groups
                |> List.sortBy (\gu -> .lastname (.tutor (Tuple.first gu)))

        allGroups =
            List.map Tuple.first allGroupsWithUsers
    in
    rContainer <|
        (allGroupsWithUsers
            |> List.map
                (\gu ->
                    showGroup sharedState model (Tuple.first gu) allGroups (Tuple.second gu)
                )
        )
            ++ [ div [ classes [ TC.ph4 ] ] <|
                    [ rRowHeaderActionButtons "Create group"
                        Styles.listHeadingStyle
                        [ ( "Create", CreateGroup model.course_id, Styles.buttonGreenStyle )
                        ]
                    ]
               ]
            ++ [ rRowButton <|
                    PbbButton <|
                        PbbActive "Send E-Mail To Course" <|
                            SendMailToCourse model.course_id
               ]


showGroup : SharedState -> Model -> Group -> List Group -> List UserEnrollment -> Html Msg
showGroup sharedState model group allGroups participants =
    div [ classes [ TC.ph4 ] ] <|
        [ rRowHeaderActionButtons ("Group - " ++ group.tutor.firstname ++ " " ++ group.tutor.lastname)
            Styles.listHeadingStyle
            [ ( "Edit", EditGroup model.course_id group.id, Styles.buttonGreyStyle )
            , ( "Users", ToggleUsers group.id, Styles.buttonGreyStyle )
            , ( "Mail", SendMailToGroup group.courseId group.id, Styles.buttonGreyStyle )
            ]
        ]
            ++ (if model.userVisibleForGroup == Just group.id then
                    [ showUserList sharedState group allGroups participants
                    ]

                else
                    []
               )


showUserList : SharedState -> Group -> List Group -> List UserEnrollment -> Html Msg
showUserList sharedState currentGroup allGroups users =
    rContainer <|
        (users
            |> List.sortBy (\u -> u.user.lastname)
            |> List.map
                (\user ->
                    showUser sharedState currentGroup allGroups user
                )
        )


showUser : SharedState -> Group -> List Group -> UserEnrollment -> Html Msg
showUser sharedState currentGroup allGroups userEnrollment =
    div [ classes [ TC.flex, TC.flex_wrap, TC.items_center, TC.pa3, TC.ph5_l ] ]
        [ UserView.view sharedState (Tuple.first <| UserView.initFromUser userEnrollment.user) Nothing
            |> Html.map WriteEmailToUser
        , div [ classes [ TC.ml4_l, TC.ml0, TC.mt0_l, TC.mt2, TC.flex ] ]
            [ multiButton <|
                (allGroups
                    |> List.map
                        (\g ->
                            ( g.tutor.firstname ++ " " ++ g.tutor.lastname
                            , g.id == currentGroup.id
                            , ReassignUser userEnrollment.user currentGroup g
                            )
                        )
                )
            ]
        ]


updateReasignUserResponse : Model -> List Int -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateReasignUserResponse model groupChangedIds response =
    case response of
        Success _ ->
            ( model, performUserEnrollmentRequestForGroups model groupChangedIds, NoUpdate )

        _ ->
            ( model, Cmd.none, NoUpdate )


fillGroupDictFromRequest : Model -> Model
fillGroupDictFromRequest model =
    case ( model.groupsRequest, Dict.isEmpty model.enrollmentRequests ) of
        ( Success groups, False ) ->
            groups
                |> List.map
                    (\g ->
                        Dict.get g.id model.enrollmentRequests
                            |> Maybe.map RemoteData.toMaybe
                            -- Convert remotedata to maybes
                            |> ME.join
                            -- we now have nested maybes and this wraps them in a single maybe
                            |> Maybe.map (\ue -> ( g, ue ))
                    )
                |> ME.values
                -- Filter out all nothings and unwrap the justs
                |> List.map (\ue -> ( .id (Tuple.first ue), ue ))
                |> Dict.fromList
                |> (\m groupEnrollmentDict -> { m | groups = groupEnrollmentDict }) model

        ( _, _ ) ->
            model


changeUsersGroup : Model -> User -> Group -> Group -> Cmd Msg
changeUsersGroup model user oldGroup newGroup =
    let
        enrollmentUpdates =
            [ oldGroup.id, newGroup.id ]

        enrollmentRequestBody =
            { userId = user.id }
    in
    GroupRequests.groupsEnrollmentPost
        model.course_id
        newGroup.id
        enrollmentRequestBody
        (ReassignUserResponse enrollmentUpdates)


performUserEnrollmentRequests : Model -> Cmd Msg
performUserEnrollmentRequests model =
    case model.groupsRequest of
        Success groups ->
            groups
                |> List.map (\g -> g.id)
                |> performUserEnrollmentRequestForGroups model

        _ ->
            Cmd.none


performUserEnrollmentRequestForGroups : Model -> List Int -> Cmd Msg
performUserEnrollmentRequestForGroups model groupIds =
    groupIds
        |> List.map (\id -> GroupRequests.groupsEnrollmentGetAll model.course_id id (GetEnrollmentResponse id))
        |> Cmd.batch
