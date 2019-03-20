module Components.Groups.BiddingView exposing 
    ( Msg(..)
    , Model
    )
{-| Group view for unassigned Students:
    - List group with time/dates and tutor
    - Set a preference (1-10) for a group
-}


import Api.Data.Group exposing (Group)
import Api.Data.GroupBid exposing (GroupBid)
import Api.Request.Courses as CourseRequests
import Components.CommonElements exposing
    ( inputElement
    , sliderInputElement
    , rRow
    , rRowHeader
    , r1Column
    , r3Column
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
import Debounce exposing (Debounce)
import Utils.Utils exposing (perform, handleLogoutErrors, split)


type Field = Rating Int


type Msg
    = GetGroupsResponse (WebData (List Group))
    | GetOldGroupBidsResponse (WebData (List GroupBid))
    | GetGroupBidResponse (WebData ())
    | SendBid Int Int
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
initGroupMsgHandler maybeGroup  maybeBid =
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
                        (Maybe.map (\_ ->
                            case (maybeGroup, maybeBid) of
                                (Just group, _) ->
                                    { gmhs | group = Just group }

                                (_, Just bid) ->
                                    { gmhs | bid = Just bid }

                                (_, _) ->
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
    { currentUserId : Int
    , courseId : Int
    , groupResponse : WebData (List Group)
    , groupOldBidsResponse : WebData (List GroupBid)
    , groupMsgHandlers : Dict Int GroupMsgHandler
    }


init : Int -> Int -> (Model, Cmd Msg)
init userId courseId =
    (
        { currentUserId = userId
        , courseId = courseId
        , groupResponse = Loading
        , groupOldBidsResponse = Loading
        , groupMsgHandlers = Dict.empty
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
            (model, Cmd.none, NoUpdate)

        SendBid groupId bid ->
            ( model 
            , CourseRequests.coursesBidsPost 
                model.courseId 
                (createGroupBid model groupId bid)
                GetGroupBidResponse
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
                    (
                        { model 
                            | groupMsgHandlers = Dict.update 
                                groupId 
                                (Maybe.map (\_ -> { gmhs | ratingDebounce = debounce } )) 
                                model.groupMsgHandlers
                        }
                    , cmd
                    , NoUpdate
                    )
                    

                Nothing ->
                    (model, Cmd.none, NoUpdate)


updateHandleGetGroupsResponse : SharedState -> Model -> WebData (List Group) -> (Model, Cmd Msg, SharedStateUpdate)
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
                    ( { model | groupResponse = response}, Cmd.none, NoUpdate)
                )
                err

        _ ->
            (model, Cmd.none, NoUpdate)


updateHandleGetOldBidsResponse : SharedState -> Model -> WebData (List GroupBid) -> (Model, Cmd Msg, SharedStateUpdate)
updateHandleGetOldBidsResponse sharedState model response =
    case response of
        Success bids ->
            ( List.foldl
                (\b m -> updateOrInitGroupMsgHandler m b.groupId Nothing (Just b.bid) )
                { model | groupOldBidsResponse = response }
                bids
            , Cmd.none
            , NoUpdate
            )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    ( { model | groupOldBidsResponse = response}, Cmd.none, NoUpdate)
                )
                err
        _ ->
            (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        groupChunks = model.groupMsgHandlers |>
            Dict.values |>
                split 3 
    in
    text ""
    {-rContainer <|
        List.map 
            (\gbmsChunk -> 
                case List.length gbmsChunk of
                    3 -> 
                        r3Column <|

                    2 ->
                        r2Column <|


                    1 ->
                        r1Column <|

                    _ -> text ""
            ) 
            groupChunks-}


viewGroupBid : SharedState -> GroupMsgHandler -> Html Msg
viewGroupBid sharedState data =
    text ""
    -- Show Tutor, Description

createGroupBid : Model -> Int -> Int -> GroupBid
createGroupBid model groupId bid =
    { id = 0
    , groupId = groupId
    , userId = model.currentUserId
    , bid = bid
    }