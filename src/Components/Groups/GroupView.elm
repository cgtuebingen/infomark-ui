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
import Dict exposing (Dict)
import Html exposing (..)
import Material
import Material.List as Lists
import Material.Options as Options exposing (css, styled, when)
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
    , summaries : Dict Int GroupSummary
    , mdc : Material.Model Msg
    }


init : Int -> List Group -> List Group -> CourseRole -> Dict Int GroupSummary -> ( Model, Cmd Msg )
init courseId ownGroups allGroups role summaries =
    ( { ownGroups = ownGroups
      , role = role
      , group = List.head ownGroups
      , allGroups = allGroups
      , courseId = courseId
      , summaries = summaries
      , mdc = Material.defaultModel
      }
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | OverwriteGroup Group
    | Mdc (Material.Msg Msg)


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        OverwriteGroup group ->
            ( { model | group = Just <| group }, Cmd.none, NoUpdate )

        Mdc msg_ ->
            let
                ( newModel, newCommand ) =
                    Material.update Mdc msg_ model
            in
            ( newModel, newCommand, NoUpdate )


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
                                , CE.rContainer <|
                                    let
                                        maybeSummary =
                                            Dict.get group.id model.summaries
                                    in
                                    case maybeSummary of
                                        Nothing ->
                                            [ h3 [ Styles.labelStyle ]
                                                [ text
                                                    "Your Students"
                                                ]
                                            , span [ Styles.textStyle ]
                                                [ text
                                                    ("No overview availiable in"
                                                        ++ " group "
                                                        ++ String.fromInt group.id
                                                        ++ " -  Number of Summaries availiable: "
                                                        ++ String.fromInt
                                                            (List.length
                                                                (Dict.keys
                                                                    model.summaries
                                                                )
                                                            )
                                                    )
                                                ]
                                            ]

                                        Just summary ->
                                            let
                                                sheets =
                                                    summary.sheets

                                                achievements =
                                                    summary.achievements
                                            in
                                            [ table []
                                                ([ caption [ Styles.labelStyle ]
                                                    [ text "Your Students" ]
                                                 , tr [ Styles.textStyle ]
                                                    ([ th [] [ text "Student" ] ]
                                                        ++ List.map
                                                            (\sheet ->
                                                                th
                                                                    []
                                                                    [ text sheet.name ]
                                                            )
                                                            sheets
                                                    )
                                                 ]
                                                    ++ List.map
                                                        (\achievement ->
                                                            tr [ Styles.textStyle ]
                                                                ([ td []
                                                                    [ text
                                                                        (achievement.user_info.first_name
                                                                            ++ " "
                                                                            ++ achievement.user_info.last_name
                                                                        )
                                                                    ]
                                                                 ]
                                                                    ++ List.map
                                                                        (\point ->
                                                                            td []
                                                                                [ text
                                                                                    (String.fromInt
                                                                                        point
                                                                                    )
                                                                                ]
                                                                        )
                                                                        achievement.points
                                                                )
                                                        )
                                                        achievements
                                                )
                                            , viewStudentTable model summary
                                            ]
                                , CE.rRowButton <|
                                    CE.PbbButton <|
                                        CE.PbbActive "Send E-Mail To Group" <|
                                            NavigateTo <|
                                                MailToGroupRoute model.courseId group.id
                                ]
                        )
                )


viewStudentTable : Model -> GroupSummary -> Html Msg
viewStudentTable model summary =
    let
        achivements =
            summary.achievements
    in
    Lists.ul Mdc
        "tutor-student-overview"
        model.mdc
        (Lists.twoLine
            :: [ css "max-width" "300px"
               , css "border" "1px solid rgba(0,0,0,.1)"
               ]
        )
        (List.map
            (\achivement ->
                Lists.li []
                    [ Lists.text []
                        [ Lists.primaryText []
                            [ text
                                (achivement.user_info.first_name
                                    ++ " "
                                    ++ achivement.user_info.last_name
                                )
                            ]
                        , Lists.secondaryText []
                            [ text "emal@todo.de" ]
                        ]
                    ]
            )
            achivements
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
