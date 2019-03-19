module Pages.MailEditor exposing 
    ( Model
    , Msg(..)
    , initForUser
    , initForGroup
    , initForCourse
    , update
    , view
    )

{-|
   This views enables writing emails
       - Should be possible for single persons, groups and courses
-}

import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import Api.Data.User as User exposing (User)
import Api.Data.Group as Group exposing (Group)
import Api.Data.Course as Course exposing (Course)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Dict exposing (Dict)
import Html exposing (..)


type MailMode
    = User (Dict Int (WebData User) )
    | Group Int Int (WebData Group)
    | Course Int (WebData Course)


type alias Model =
    { mode : MailMode
    , message : String
    }


initModelFromMode : MailMode -> Model
initModelFromMode mode =
    { mode = mode
    , message = ""
    }


initForUser : Int -> (Model, Cmd Msg)
initForUser userId =
    ( initModelFromMode <| User <| Dict.fromList [ (userId, Loading) ]
    , Cmd.none
    )


initForGroup : Int -> Int -> (Model, Cmd Msg)
initForGroup courseId groupId =
    ( initModelFromMode <| Group courseId groupId Loading
    , Cmd.none
    )


initForCourse : Int -> (Model, Cmd Msg)
initForCourse courseId =
    ( initModelFromMode <| Course courseId Loading
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | SendMessage
    | SendMessageResponse (WebData ())
    | AddUser User
    | SearchUserByMailResponse (WebData User)
    | UserResponse (WebData User)
    | GroupResponse (WebData Group)
    | CourseResponse (WebData Course)


update : SharedState -> Msg -> Model -> (Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        NavigateTo route ->
            (model, Cmd.none, NoUpdate)
        
        SendMessage ->
            (model, Cmd.none, NoUpdate)

        SendMessageResponse response ->
            (model, Cmd.none, NoUpdate)

        AddUser user ->
            (model, Cmd.none, NoUpdate)

        SearchUserByMailResponse response ->
            (model, Cmd.none, NoUpdate)

        UserResponse response ->
            (model, Cmd.none, NoUpdate)
            
        GroupResponse response ->
            (model, Cmd.none, NoUpdate)

        CourseResponse response ->
            (model, Cmd.none, NoUpdate)

view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []