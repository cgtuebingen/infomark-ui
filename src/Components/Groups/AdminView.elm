module Components.Groups.AdminView exposing (..)
{-| Group view for Admins:
    - Options to create/edit/delete groups
    - Options to view all users in a group
    - Option to reassign users to a different group 
-}

import Api.Data.Group exposing (Group)
import Api.Data.User exposing (User)
Api.Data.GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Request.Groups as GroupRequests
import Api.Request.Courses as CourseRequests
import Components.CommonElements exposing
    ( inputElement
    , rRow
    , r1Column
    , r2Column
    , rRowHeaderActionButtons
    , rContainer
    , rRowButton
    )
import Components.UserAvatarEmailView as UserView
import Html exposing (..)
import Html.Attributes exposing (..)
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Dict exposing (Dict)


type Msg
    = GetGroupsResponse (WebData (List Group))
    | SendGroup Int Int
    | ToggleUsers Int
    | ReassignUser User Group
    | SendGroupResponse Int Int (WebData ())
    | ReassignUserResponse (WebData ())


type alias GroupDetail =
    { users : Maybe (List User)
    , tutor : Maybe User
    , group : Group
    }

type alias Model =
    { course_id : Int
    , groupsRequest : WebData (List Group)
    , groups : Dict Int Group
    }


init : Int -> (Model, Cmd Msg)
init courseId =
    (
        { course_id = courseId
        , groupsRequest = Loading
        , groups = Dict.empty
        }
    , CourseRequests.courseGroupsGet courseId GetGroupsResponse
    )