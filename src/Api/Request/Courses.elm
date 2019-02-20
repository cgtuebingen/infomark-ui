module Api.Request.Courses exposing (courseDelete, courseGet, courseGroupsGet, courseGroupsPost, courseOwnGroupGet, coursePatch, coursesGet, coursesPost)

import Api.Data.Course as Course exposing (Course)
import Api.Data.Error as Error exposing (Error)
import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupBid as GroupBid exposing (GroupBid)
import Api.Endpoint exposing (course, courseGroup, courseGroupBids, courseGroups, courses, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


coursesPost : Course -> (WebData Course -> msg) -> Cmd msg
coursesPost courseNew msg =
    post (unwrap courses)
        (Http.jsonBody (Course.encoder courseNew))
        msg
        Course.decoder


coursesGet : (WebData (List Course) -> msg) -> Cmd msg
coursesGet msg =
    get (unwrap courses)
        msg
    <|
        Decode.list Course.decoder


courseGet : Int -> (WebData Course -> msg) -> Cmd msg
courseGet id msg =
    get (unwrap <| course id)
        msg
        Course.decoder


coursePatch : Int -> Course -> (WebData String -> msg) -> Cmd msg
coursePatch id courseUp msg =
    patch (unwrap <| course id)
        (Http.jsonBody (Course.encoder courseUp))
        msg
        Decode.string


courseDelete : Int -> (WebData String -> msg) -> Cmd msg
courseDelete id msg =
    delete (unwrap <| course id)
        msg
        Decode.string


courseGroupsGet : Int -> (WebData (List Group) -> msg) -> Cmd msg
courseGroupsGet id msg =
    get (unwrap <| courseGroups id)
        msg
    <|
        Decode.list Group.decoder


courseGroupsPost : Int -> Group -> (WebData Group -> msg) -> Cmd msg
courseGroupsPost id groupNew msg =
    post (unwrap <| courseGroups id)
        (Http.jsonBody (Group.encoder groupNew))
        msg
        Group.decoder


courseOwnGroupGet : Int -> (WebData Group -> msg) -> Cmd msg
courseOwnGroupGet id msg =
    get (unwrap <| courseGroup id)
        msg
        Group.decoder


coursesBidsGet : Int -> (WebData (List GroupBid) -> msg) -> Cmd msg
coursesBidsGet id msg =
    get (unwrap <| courseGroupBids id)
        msg
    <|
        Decode.list GroupBid.decoder


coursesBidsPost : Int -> GroupBid -> (WebData GroupBid -> msg) -> Cmd msg
coursesBidsPost id groupBidNew msg =
    post (unwrap <| courseGroupBids id)
        (Http.jsonBody (GroupBid.encoder groupBidNew))
        msg
        GroupBid.decoder
