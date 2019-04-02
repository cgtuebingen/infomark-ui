module Pages.GroupEditor exposing
    ( Model
    , Msg(..)
    , initCreate
    , initEdit
    , initEditFromId
    , update
    , view
    )

import Api.Data.Group exposing (Group)
import Api.Data.User exposing (User)
import Api.Data.UserEnrollment exposing (UserEnrollment)
import Api.Request.Courses as CourseRequests
import Api.Request.Groups as GroupRequests
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbState(..)
        , normalPage
        , pageContainer
        , r1Column
        , r2Column
        , rContainer
        , rRow
        , rRowButton
        , rRowHeader
        , searchElement
        , textAreaElement
        )
import Components.UserAvatarEmailView as UserView
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors, perform)


type Field
    = Description
    | SearchByMail


type Msg
    = NavigateTo Route
    | SearchUserByEmail
    | SearchUserResponse (WebData (List UserEnrollment))
    | SetUserAsTutor User
    | SetField Field String
    | CreateOrEdit
    | CreateResponse (WebData Group)
    | EditResponse (WebData ())
    | GetGroupResponse (WebData Group)
    | WriteEmailMsg Int


type alias Model =
    { courseId : Int
    , maybeTutor : Maybe User
    , description : String
    , groupId : Int
    , searchByEmailInput : String
    , searchResponse : WebData (List UserEnrollment)
    , createGroup : Bool
    , errors : List ( Field, String )
    }


initModel : Model
initModel =
    { courseId = 0
    , maybeTutor = Nothing
    , description = ""
    , groupId = 0
    , searchByEmailInput = ""
    , searchResponse = NotAsked
    , createGroup = False
    , errors = []
    }


initCreate : Int -> ( Model, Cmd Msg )
initCreate courseId =
    ( { initModel | courseId = courseId, createGroup = True }, Cmd.none )


initEdit : Int -> Group -> ( Model, Cmd Msg )
initEdit courseId group =
    ( fillModelFromGroup initModel group, Cmd.none )


initEditFromId : Int -> Int -> ( Model, Cmd Msg )
initEditFromId courseId groupId =
    ( initModel
    , GroupRequests.groupsGet courseId groupId GetGroupResponse
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        GetGroupResponse (Success group) ->
            ( fillModelFromGroup model group, Cmd.none, NoUpdate )

        GetGroupResponse (Failure err) ->
            ( model, perform <| NavigateTo <| CourseDetailRoute model.courseId, NoUpdate )

        GetGroupResponse _ ->
            ( model, Cmd.none, NoUpdate )

        SearchUserByEmail ->
            ( { model | searchResponse = Loading }
            , CourseRequests.coursesEnrollmentGetByEmail model.courseId model.searchByEmailInput SearchUserResponse
            , NoUpdate
            )

        SearchUserResponse response ->
            ( { model | searchResponse = response }, Cmd.none, NoUpdate )

        SetUserAsTutor user ->
            ( { model | maybeTutor = Just user }, Cmd.none, NoUpdate )

        SetField field value ->
            ( setField field value model
            , Cmd.none
            , NoUpdate
            )

        CreateOrEdit ->
            case model.maybeTutor of
                Just tutor ->
                    ( model
                    , if model.createGroup then
                        CourseRequests.courseGroupsPost
                            model.courseId
                            (fillRequestFromModel model tutor)
                            CreateResponse

                      else
                        GroupRequests.groupsPut
                            model.courseId
                            model.groupId
                            (fillRequestFromModel model tutor)
                            EditResponse
                    , NoUpdate
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        CreateResponse response ->
            updateHandleCreateOrEditResponse model response

        EditResponse response ->
            updateHandleCreateOrEditResponse model response

        WriteEmailMsg userId ->
            -- Currently not used
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    pageContainer
        [ normalPage
            [ rContainer <|
                [ rRowHeader <|
                    if model.createGroup then
                        "Gruppe erstellen"

                    else
                        "Gruppe bearbeiten"
                , case model.maybeTutor of
                    Just tutor ->
                        rRow <|
                            r2Column
                                [ h1 [ Styles.headerStyle ] [ text "Tutor" ] ]
                                [ UserView.view sharedState (Tuple.first <| UserView.initFromUser tutor) Nothing
                                    |> Html.map WriteEmailMsg
                                ]

                    _ ->
                        text ""
                , rRow <|
                    r2Column
                        [ h1 [ Styles.headerStyle ] [ text "Search For Tutor" ]
                        ]
                        (searchElement
                            { placeholder = "Search by E-Mail"
                            , fieldType = "email"
                            , value = model.searchByEmailInput
                            }
                            SearchByMail
                            SetField
                            SearchUserByEmail
                        )
                , rRow <|
                    [ case model.searchResponse of
                        Success userEnrollments ->
                            case List.head userEnrollments of
                                Just userEnrollment ->
                                    let
                                        user =
                                            userEnrollment.user

                                        avatar =
                                            case user.avatarUrl of
                                                Just avatarUrl ->
                                                    avatarUrl

                                                Nothing ->
                                                    "images/defaultAvatar.png"
                                    in
                                    div [ classes [ TC.flex, TC.flex_wrap, TC.items_center, TC.pa3, TC.ph5_l ] ]
                                        [ UserView.view sharedState (Tuple.first <| UserView.initFromUser user) Nothing
                                            |> Html.map WriteEmailMsg
                                        , div [ classes [ TC.ml4_l, TC.ml0, TC.mt0_l, TC.mt2, TC.flex ] ]
                                            [ button
                                                [ Styles.buttonGreenStyle
                                                , onClick <| SetUserAsTutor user
                                                ]
                                                [ text "Set as tutor" ]
                                            ]
                                        ]

                                _ ->
                                    h2
                                        [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph3, TC.ph5_ns ]
                                        , Styles.listHeadingStyle
                                        ]
                                        [ text "Not found" ]

                        _ ->
                            text ""
                    ]
                , rRow <|
                    r1Column <|
                        textAreaElement
                            { label = "Description"
                            , placeholder = "Details about place and time of group meetings"
                            , value = model.description
                            }
                            Description
                            model.errors
                            SetField
                , rRowButton <|
                    PbbButton <|
                        case model.maybeTutor of
                            Just _ ->
                                PbbActive
                                    (if model.createGroup then
                                        "Erstellen"

                                     else
                                        "Bearbeiten"
                                    )
                                    CreateOrEdit

                            _ ->
                                PbbDisabled
                                    (if model.createGroup then
                                        "Erstellen"

                                     else
                                        "Bearbeiten"
                                    )
                ]
            ]
        ]


updateHandleCreateOrEditResponse : Model -> WebData resp -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleCreateOrEditResponse model response =
    case response of
        Success _ ->
            ( model, perform <| NavigateTo <| CourseDetailRoute model.courseId, NoUpdate )

        Failure err ->
            ( model, Cmd.none, NoUpdate )

        -- TODO: Show error message
        _ ->
            ( model, Cmd.none, NoUpdate )


setField : Field -> String -> Model -> Model
setField field value model =
    case field of
        Description ->
            { model | description = value }

        SearchByMail ->
            { model | searchByEmailInput = value }


fillRequestFromModel : Model -> User -> Group
fillRequestFromModel model tutor =
    { courseId = model.courseId
    , description = model.description
    , tutor = tutor
    , id = model.groupId
    }


fillModelFromGroup : Model -> Group -> Model
fillModelFromGroup model group =
    { model
        | groupId = group.id
        , description = group.description
        , courseId = group.courseId
        , maybeTutor = Just group.tutor
    }
