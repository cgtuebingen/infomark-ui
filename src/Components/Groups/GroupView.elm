module Components.Groups.GroupView exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

{-| Group view for assigned Students and Tutors:

  - Show your group with time/date and members
  - If you are a Tutor: Link to send email to all members

-}

import Api.Data.CourseRole exposing (CourseRole(..))
import Api.Data.Group exposing (Group)
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { ownGroups : List Group
    , group : Maybe Group
    , role : CourseRole
    , allGroups : List Group
    }


init : List Group -> List Group -> CourseRole -> ( Model, Cmd Msg )
init ownGroups allGroups role =
    ( { ownGroups = ownGroups
      , role = role
      , group = Nothing
      , allGroups = allGroups
      }
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | OverwriteGroup Group


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        OverwriteGroup group ->
            ( { model | group = Just <| group }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    text "Student/Tutor View"
