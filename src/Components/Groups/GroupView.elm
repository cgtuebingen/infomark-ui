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
import Api.Data.GroupSummary exposing (GroupSummary)
import Api.Data.User exposing (User)
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements as CE
import Components.UserAvatarEmailView as UserView
import Html exposing (..)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles


type alias Model =
    { ownGroups : List Group
    , group : Maybe Group
    , role : CourseRole
    , allGroups : List Group
    , courseId : Int
    , summaries : List GroupSummary
    }


init : Int -> List Group -> List Group -> CourseRole -> List GroupSummary -> ( Model, Cmd Msg )
init courseId ownGroups allGroups role summaries =
    ( { ownGroups = ownGroups
      , role = role
      , group = List.head ownGroups
      , allGroups = allGroups
      , courseId = courseId
      , summaries = summaries
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
    case ( model.role, model.group ) of
        ( Student, Just group ) ->
            CE.rContainer <|
                [ CE.rRow <|
                    CE.r2Column
                        [ CE.rContainer <|
                            [ h3 [ Styles.labelStyle ] [ text "Tutor" ]
                            , UserView.view
                                sharedState
                                (Tuple.first <| UserView.initFromUser group.tutor)
                                Nothing
                            ]
                        ]
                        [ CE.rContainer <|
                            [ h3 [ Styles.labelStyle ] [ text "Gruppeninfo" ]
                            , span [ Styles.textStyle ] [ text group.description ]
                            ]
                        ]
                ]

        ( _, _ ) ->
            CE.rContainer <|
                (model.ownGroups
                    |> List.map
                        (\group ->
                            CE.rContainer <|
                                [ CE.rRow <|
                                    CE.r2Column
                                        [ CE.rContainer <|
                                            [ h3 [ Styles.labelStyle ] [ text "Tutor" ]
                                            , UserView.view
                                                sharedState
                                                (Tuple.first <| UserView.initFromUser group.tutor)
                                                Nothing
                                            ]
                                        ]
                                        [ CE.rContainer <|
                                            [ h3 [ Styles.labelStyle ] [ text "Gruppeninfo" ]
                                            , span [ Styles.textStyle ] [ text group.description ]
                                            ]
                                        ]
                                , CE.rRow <|
                                    [ h3 [ Styles.labelStyle ] [ text "Students" ]
                                    , span [ Styles.textStyle ]
                                        [ text "Here will be a overview over your students soon." ]
                                    ]
                                , CE.rRowButton <| CE.PbbButton <| CE.PbbActive "Send E-Mail To Group" <| NavigateTo <| MailToGroupRoute model.courseId group.id
                                ]
                        )
                )


viewGroupMembers : SharedState -> List User -> Html Msg
viewGroupMembers sharedState users =
    let
        sortedUsers =
            users
                |> List.sortBy (\u -> u.lastname)
    in
    CE.rContainer <|
        [ div [ classes [ TC.flex, TC.flex_row, TC.flex_wrap, TC.justify_start ] ]
            (sortedUsers
                |> List.map (\ue -> UserView.initFromUser ue)
                |> List.map Tuple.first
                |> List.map
                    (\uv ->
                        div
                            [ classes
                                [ TC.w_third_l
                                , TC.w_50_m
                                , TC.w_100
                                ]
                            ]
                            [ UserView.view sharedState uv Nothing ]
                    )
            )
        ]
