module Api.Request.Groups exposing
    ( groupsDelete
    , groupsEnrollmentPost
    , groupsEnrollmentPut
    , groupsGet
    , groupsPut
    )

import Api.Data.Error as Error exposing (Error)
import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupEnrollmentChange as GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Endpoint exposing (groups, groupsEnrollment, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


groupsGet : Int -> (WebData Group -> msg) -> Cmd msg
groupsGet id msg =
    get (unwrap <| groups id)
        msg
        Group.decoder


groupsPut : Int -> Group -> (WebData String -> msg) -> Cmd msg
groupsPut id group msg =
    put (unwrap <| groups id)
        (Http.jsonBody (Group.encoder group))
        msg
        Decode.string


groupsDelete : Int -> (WebData String -> msg) -> Cmd msg
groupsDelete id msg =
    delete (unwrap <| groups id)
        msg
        Decode.string


groupsEnrollmentPut : Int -> GroupEnrollmentChange -> (WebData String -> msg) -> Cmd msg
groupsEnrollmentPut id groupEnrollment msg =
    put (unwrap <| groupsEnrollment id)
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg
        Decode.string


groupsEnrollmentPost : Int -> GroupEnrollmentChange -> (WebData Group -> msg) -> Cmd msg
groupsEnrollmentPost id groupEnrollment msg =
    post (unwrap <| groupsEnrollment id)
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg
        Group.decoder
