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
import Api.Data.Mail as Mail exposing (Mail)
import Api.Data.User as User exposing (User)
import Api.Request.Courses as CourseRequest
import Api.Request.Groups as GroupRequest
import Api.Request.Mail exposing (sendCourseMailPost, sendGroupMailPost)
import Api.Request.User as UserRequest
import Browser.Navigation exposing (pushUrl)
import Components.Toasty
import Dict exposing (Dict)
import Html exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty


type MailMode
    = User (Dict Int (WebData User))
    | Group Int Int (WebData Group)
    | Course Int (WebData Course)


type alias Model =
    { mode : MailMode
    , message : String
    , subject : String
    , toasties : Toasty.Stack Components.Toasty.Toast
    }


initModelFromMode : MailMode -> Model
initModelFromMode mode =
    { mode = mode
    , message = ""
    , subject = ""
    , toasties = Toasty.initialState
    }



-- Mail to User route: not relevant right now ignore that


initForUser : Int -> ( Model, Cmd Msg )
initForUser userId =
    ( initModelFromMode <| User <| Dict.fromList [ ( userId, Loading ) ]
    , UserRequest.userGet userId UserResponse
    )



-- Mail to group route: Tutors send mail to one of their groups, groupId given
-- Retrive group for meta data like group name


initForGroup : Int -> Int -> ( Model, Cmd Msg )
initForGroup courseId groupId =
    ( initModelFromMode <| Group courseId groupId Loading
    , GroupRequest.groupsGet courseId groupId GroupResponse
    )



-- Mail to course route: Admins can send the entire course a Mail.
-- Just need to get the CourseId


initForCourse : Int -> ( Model, Cmd Msg )
initForCourse courseId =
    ( initModelFromMode <| Course courseId Loading
    , CourseRequest.courseGet courseId CourseResponse
    )


type Msg
    = NavigateTo Route -- route somewhere else
    | SendMessage -- tell the server to send a mail
    | SendMessageResponse (WebData ()) -- the response of the server to a send request
    | AddUser User -- add a user to which you want to send stuff... ignore for now
    | SearchUserByMailResponse (WebData User) -- ignore for now
    | UserResponse (WebData User) -- Response for user request (init)
    | GroupResponse (WebData Group) -- Response for group request (init)
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | CourseResponse (WebData Course) -- Response for Course request (init)


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SendMessage ->
            case model.mode of
                -- user email not supported yet
                User users ->
                    ( model
                    , Cmd.none
                    , NoUpdate
                    )

                Group courseId groupId group ->
                    ( model
                    , sendGroupMailPost courseId
                        groupId
                        (modelToMail model)
                        SendMessageResponse
                    , NoUpdate
                    )

                Course courseId course ->
                    ( model
                    , sendCourseMailPost courseId
                        (modelToMail model)
                        SendMessageResponse
                    , NoUpdate
                    )

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config
                        ToastyMsg
                        subMsg
                        model
            in
            ( newModel, newCmd, NoUpdate )

        SendMessageResponse response ->
            updateHandleSendMessageResponse sharedState model response

        -- not supported yet
        AddUser user ->
            ( model, Cmd.none, NoUpdate )

        -- not supported yet
        SearchUserByMailResponse response ->
            ( model, Cmd.none, NoUpdate )

        -- not supported yet
        UserResponse response ->
            ( model, Cmd.none, NoUpdate )

        GroupResponse response ->
            ( model, Cmd.none, NoUpdate )

        CourseResponse response ->
            ( model, Cmd.none, NoUpdate )


updateHandleSendMessageResponse : SharedState -> Model -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleSendMessageResponse sharedState model response =
    case response of
        -- Go back to dashboard
        Success _ ->
            ( model
            , pushUrl sharedState.navKey
                (reverseRoute
                    DashboardRoute
                )
            , NoUpdate
            )

        Failure err ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> addToast
                            (Components.Toasty.Error "Error"
                                "Failed to deliver Email"
                            )
            in
            ( newModel, newCmd, NoUpdate )

        _ ->
            ( model, Cmd.none, NoUpdate )


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )


modelToMail : Model -> Mail
modelToMail model =
    { subject = model.subject
    , message = model.message
    }



-- Update tile in model


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] []
