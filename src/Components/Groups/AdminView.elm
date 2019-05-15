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
        , nButtonList
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
import Html.Events exposing (onClick)
import Material
import Material.IconButton as IconButton
import Material.List as Lists
import Material.Options as Options exposing (css, styled, when)
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
    , mdc : Material.Model Msg
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
            , mdc = Material.defaultModel
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
        showGroups model allGroups
            ++ [ rRowButton <|
                    PbbButton <|
                        PbbActive "Send E-Mail To Course" <|
                            SendMailToCourse model.course_id
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


showGroups : Model -> List Group -> List (Html Msg)
showGroups model allGroups =
    [ nButtonList
        (List.map
            (\group ->
                let
                    tutor =
                        group.tutor
                in
                { button1_icon = "edit"
                , button1_msg = EditGroup model.course_id group.id
                , right_buttons =
                    [ { button_icon = "mail"
                      , button_msg = SendMailToGroup model.course_id group.id
                      }
                    ]
                , label = tutor.firstname ++ " " ++ tutor.lastname
                }
            )
            allGroups
        )
    ]
