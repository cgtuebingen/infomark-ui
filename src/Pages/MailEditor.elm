module Pages.MailEditor exposing
    ( Model
    , Msg(..)
    , initForCourse
    , initForGroup
    , initForUser
    , update
    , view
    )

{-| This views enables writing emails

  - Should be possible for single persons, groups and courses

-}

import Api.Data.Course as Course exposing (Course)
import Api.Data.Group as Group exposing (Group)
import Api.Data.User as User exposing (User)
import Dict exposing (Dict)
import Html exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))


type MailMode
    = User (Dict Int (WebData User))
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



-- Mail to User route: not relevant right now ignore that


initForUser : Int -> ( Model, Cmd Msg )
initForUser userId =
    ( initModelFromMode <| User <| Dict.fromList [ ( userId, Loading ) ]
    , Cmd.none
    )



-- Mail to group route: Tutors send their students mails
-- Retrive all Groups
-- In the update thingy all groups need to be filtered for the groups of the
-- tutor and the GroupIds to be stored in the model.
-- Is the message lost after some inactivity???


initForGroup : Int -> Int -> ( Model, Cmd Msg )
initForGroup courseId groupId =
    ( initModelFromMode <| Group courseId groupId Loading
    , Cmd.none
    )



-- Mail to course route: Admins can send the entire course a Mail.
-- Just need to get the CourseId


initForCourse : Int -> ( Model, Cmd Msg )
initForCourse courseId =
    ( initModelFromMode <| Course courseId Loading
    , Cmd.none
    )


type Msg
    = NavigateTo Route -- route somewhere else
    | SendMessage -- tell the server to send a mail
    | SendMessageResponse (WebData ()) -- the response of the server to a send request
    | AddUser User -- add a user to which you want to send stuff... ignore for now
    | SearchUserByMailResponse (WebData User) -- ignore for now
    | UserResponse (WebData User) -- Response for user request (init)
    | GroupResponse (WebData Group) -- Response for group request (init)
    | CourseResponse (WebData Course) -- Response for Course request (init)


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, Cmd.none, NoUpdate )

        SendMessage ->
            ( model, Cmd.none, NoUpdate )

        SendMessageResponse response ->
            ( model, Cmd.none, NoUpdate )

        AddUser user ->
            ( model, Cmd.none, NoUpdate )

        SearchUserByMailResponse response ->
            ( model, Cmd.none, NoUpdate )

        UserResponse response ->
            ( model, Cmd.none, NoUpdate )

        GroupResponse response ->
            ( model, Cmd.none, NoUpdate )

        CourseResponse response ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
