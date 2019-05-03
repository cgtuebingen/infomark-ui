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
import Html.Attributes exposing (class)
import Material
import Material.Options as Options exposing (cs, css, styled, when)
import Material.TextField as TextField
import Material.TextField.CharacterCounter as TextField
import Material.TextField.HelperLine as TextField
import Material.TextField.HelperText as TextField
import Material.Typography as Typography
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
    , studentFilter : String
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
      , studentFilter = ""
      }
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | OverwriteGroup Group
    | Mdc (Material.Msg Msg)
    | UpdateStudentFilter String


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

        UpdateStudentFilter filter ->
            ( { model | studentFilter = filter }, Cmd.none, NoUpdate )


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
                                            viewStudentSummary model summary
                                , CE.rRowButton <|
                                    CE.PbbButton <|
                                        CE.PbbActive "Send E-Mail To Group" <|
                                            NavigateTo <|
                                                MailToGroupRoute model.courseId group.id
                                ]
                        )
                )


viewStudentSummary : Model -> GroupSummary -> List (Html Msg)
viewStudentSummary model summary =
    let
        sheets =
            summary.sheets

        achievements =
            summary.achievements

        achievements_sorted =
            List.sortWith
                (\a b ->
                    compare
                        a.user_info.last_name
                        b.user_info.last_name
                )
                achievements

        achievements_filtered =
            List.filter
                (\a ->
                    String.contains
                        (String.toUpper
                            model.studentFilter
                        )
                        (String.toUpper
                            a.user_info.first_name
                        )
                        || String.contains
                            (String.toUpper
                                model.studentFilter
                            )
                            (String.toUpper
                                a.user_info.last_name
                            )
                )
                achievements_sorted

        textField index options =
            [ TextField.view Mdc
                index
                model.mdc
                (TextField.label
                    "Find Name"
                    :: options
                )
                []
            , TextField.helperLine []
                [ TextField.helperText
                    [ TextField.persistent ]
                    [ text
                        "Filter list by name - e.g. 'Alice'"
                    ]
                ]
            ]
    in
    [ h2
        []
        [ text "Your Students" ]
    , textFieldRow []
        [ textFieldContainer []
            (textField
                "text-field-student-overview-filter"
                [ Options.onChange
                    UpdateStudentFilter
                , Options.onInput
                    UpdateStudentFilter
                ]
            )
        ]
    , table [ class "striped", class "overview-table" ]
        [ thead []
            [ tr [ Styles.textStyle ]
                ([ th [ class "student-overview-head-horizontal" ]
                    [ div [] [ span [] [ text "Student" ] ] ]
                 ]
                    ++ List.map
                        (\sheet ->
                            th
                                [ class "student-overview-head-vertical" ]
                                [ div []
                                    [ span [] [ text (String.left 8 sheet.name) ] ]
                                ]
                        )
                        sheets
                )
            ]
        , tbody []
            (List.map
                (\achievement ->
                    tr [ Styles.textStyle ]
                        ([ td []
                            [ text
                                (achievement.user_info.first_name
                                    ++ " "
                                    ++ achievement.user_info.last_name
                                )
                            , br [] [ text "" ]
                            , small [] [ text achievement.user_info.email ]
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
                achievements_filtered
            )
        ]
    ]


textFieldContainer : List (Options.Property c m) -> List (Html m) -> Html m
textFieldContainer options =
    styled Html.div
        (cs "text-field-container"
            :: css "min-width" "200px"
            :: options
        )


textFieldRow : List (Options.Property c m) -> List (Html m) -> Html m
textFieldRow options =
    styled Html.div
        (cs "text-field-row"
            :: css "display" "flex"
            :: css "align-items" "flex-start"
            :: css "justify-content" "space-between"
            :: css "flex-wrap" "wrap"
            :: options
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
