module Components.Groups.BiddingView exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

{-| Group view for unassigned Students:

  - List group with time/dates and tutor
  - Set a preference (1-10) for a group

-}

import Api.Data.Group exposing (Group)
import Api.Data.GroupBid exposing (GroupBid)
import Api.Request.Courses as CourseRequests
import Components.CommonElements
    exposing
        ( inputElement
        , r1Column
        , r3Column
        , rContainer
        , rRow
        , rRowButton
        , rRowHeader
        , rRowHeaderActionButtons
        , sliderInputElement
        )
import Components.Toasty
import Components.UserAvatarEmailView as UserView
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors, perform, split)


type Field
    = Rating Int


type Msg
    = GetGroupsResponse (WebData (List Group))
    | GetOldGroupBidsResponse (WebData (List GroupBid))
    | GetGroupBidResponse (WebData ())
    | SendBid Int Int
    | SetBid Field String
    | DebounceMsg Int Debounce.Msg


debounceConfig : Int -> Debounce.Config Msg
debounceConfig id =
    { strategy = Debounce.later 2000
    , transform = DebounceMsg id
    }


type alias GroupMsgHandler =
    { group : Maybe Group
    , bid : Maybe Int
    , ratingDebounce : Debounce Int
    }


initGroupMsgHandler : Maybe Group -> Maybe Int -> GroupMsgHandler
initGroupMsgHandler maybeGroup maybeBid =
    { group = maybeGroup
    , bid = maybeBid
    , ratingDebounce = Debounce.init
    }


updateOrInitGroupMsgHandler : Model -> Int -> Maybe Group -> Maybe Int -> Model
updateOrInitGroupMsgHandler model groupId maybeGroup maybeBid =
    { model
        | groupMsgHandlers =
            case Dict.get groupId model.groupMsgHandlers of
                Just gmhs ->
                    Dict.update
                        groupId
                        (Maybe.map
                            (\_ ->
                                case ( maybeGroup, maybeBid ) of
                                    ( Just group, _ ) ->
                                        { gmhs | group = Just group }

                                    ( _, Just bid ) ->
                                        { gmhs | bid = Just bid }

                                    ( _, _ ) ->
                                        gmhs
                            )
                        )
                        model.groupMsgHandlers

                Nothing ->
                    Dict.insert
                        groupId
                        (initGroupMsgHandler maybeGroup maybeBid)
                        model.groupMsgHandlers
    }


type alias Model =
    { courseId : Int
    , groupResponse : WebData (List Group)
    , groupOldBidsResponse : WebData (List GroupBid)
    , groupMsgHandlers : Dict Int GroupMsgHandler
    , errors : List ( Field, String )
    }


init : Int -> ( Model, Cmd Msg )
init courseId =
    ( { courseId = courseId
      , groupResponse = Loading
      , groupOldBidsResponse = Loading
      , groupMsgHandlers = Dict.empty
      , errors = []
      }
    , Cmd.batch
        [ CourseRequests.courseGroupsGet courseId GetGroupsResponse
        , CourseRequests.coursesBidsGet courseId GetOldGroupBidsResponse
        ]
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        GetGroupsResponse response ->
            updateHandleGetGroupsResponse sharedState model response

        GetOldGroupBidsResponse response ->
            updateHandleGetOldBidsResponse sharedState model response

        GetGroupBidResponse response ->
            ( model, Cmd.none, NoUpdate )

        SendBid groupId bid ->
            ( model
            , CourseRequests.coursesBidsPost
                model.courseId
                groupId
                bid
                GetGroupBidResponse
            , NoUpdate
            )

        SetBid field val ->
            case field of
                Rating groupId ->
                    let
                        toInt =
                            Maybe.withDefault 0 <| String.toInt val

                        maybeDebounceCmd =
                            Dict.get groupId model.groupMsgHandlers
                                |> Maybe.map
                                    (\gms ->
                                        Debounce.push (debounceConfig groupId) toInt gms.ratingDebounce
                                    )

                        updatedGms =
                            Dict.update
                                groupId
                                (Maybe.map
                                    (\gms ->
                                        { gms
                                            | bid = Just toInt
                                            , ratingDebounce =
                                                case maybeDebounceCmd of
                                                    Just debounceCmd ->
                                                        Tuple.first debounceCmd

                                                    Nothing ->
                                                        Debounce.init
                                        }
                                    )
                                )
                                model.groupMsgHandlers
                    in
                    ( { model | groupMsgHandlers = updatedGms }
                    , Maybe.withDefault Cmd.none <| Maybe.map Tuple.second maybeDebounceCmd
                    , NoUpdate
                    )

        DebounceMsg groupId subMsg ->
            case Dict.get groupId model.groupMsgHandlers of
                Just gmhs ->
                    let
                        ( debounce, cmd ) =
                            Debounce.update
                                (debounceConfig groupId)
                                (Debounce.takeLast (\b -> perform <| SendBid groupId b))
                                subMsg
                                gmhs.ratingDebounce
                    in
                    ( { model
                        | groupMsgHandlers =
                            Dict.update
                                groupId
                                (Maybe.map (\_ -> { gmhs | ratingDebounce = debounce }))
                                model.groupMsgHandlers
                      }
                    , cmd
                    , NoUpdate
                    )

                Nothing ->
                    ( model, Cmd.none, NoUpdate )


updateHandleGetGroupsResponse : SharedState -> Model -> WebData (List Group) -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleGetGroupsResponse sharedState model response =
    case response of
        Success groups ->
            ( List.foldl
                (\g m -> updateOrInitGroupMsgHandler m g.id (Just g) Nothing)
                { model | groupResponse = response }
                groups
            , Cmd.none
            , NoUpdate
            )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    ( { model | groupResponse = response }, Cmd.none, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


updateHandleGetOldBidsResponse : SharedState -> Model -> WebData (List GroupBid) -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleGetOldBidsResponse sharedState model response =
    case response of
        Success bids ->
            ( List.foldl
                (\b m -> updateOrInitGroupMsgHandler m b.groupId Nothing (Just b.bid))
                { model | groupOldBidsResponse = response }
                bids
            , Cmd.none
            , ShowToast <| Components.Toasty.Success "Success" "Your bids have been updated"
            )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    ( { model | groupOldBidsResponse = response }, Cmd.none, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        groupChunks =
            model.groupMsgHandlers
                |> Dict.values
                |> split 3
    in
    rContainer <|
        List.map
            (\gbmsChunk ->
                case gbmsChunk of
                    c1 :: c2 :: c3 :: [] ->
                        rRow <|
                            r3Column
                                [ viewGroupBid sharedState model c1 ]
                                [ viewGroupBid sharedState model c2 ]
                                [ viewGroupBid sharedState model c3 ]

                    c1 :: c2 :: [] ->
                        rRow <|
                            r3Column
                                [ viewGroupBid sharedState model c1 ]
                                [ viewGroupBid sharedState model c2 ]
                                [ div [ classes [ TC.db, TC.w_100 ] ] [] ]

                    c1 :: [] ->
                        rRow <|
                            r3Column
                                [ viewGroupBid sharedState model c1 ]
                                [ div [ classes [ TC.db, TC.w_100 ] ] [] ]
                                [ div [ classes [ TC.db, TC.w_100 ] ] [] ]

                    _ ->
                        text ""
            )
            groupChunks


viewGroupBid : SharedState -> Model -> GroupMsgHandler -> Html Msg
viewGroupBid sharedState model data =
    case data.group of
        Just group ->
            let
                tutor =
                    group.tutor

                curBid =
                    Maybe.withDefault 1 data.bid
            in
            div [ classes [ TC.w_100, TC.ph2 ] ] <|
                [ h3 [ Styles.listHeadingStyle ]
                    [ text <| "Group - " ++ tutor.firstname ++ " " ++ tutor.lastname
                    ]
                , span [ Styles.textStyle, classes [ TC.mb3, TC.db ] ]
                    [ text <| group.description ]
                ]
                    ++ sliderInputElement
                        { label = "Präferenz (1 = Nein, 10 = Unbedingt)"
                        , value = curBid
                        , min = 1
                        , max = 10
                        , step = 1
                        , valueLabel = String.fromInt curBid
                        }
                        (Rating group.id)
                        model.errors
                        SetBid

        Nothing ->
            text ""
