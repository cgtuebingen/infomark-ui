module Api.Request.Groups exposing
    ( groupsDelete
    , groupsEnrollmentPost
    , groupsEnrollmentPut
    , groupsGet
    , groupsPut
    )

import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupEnrollmentChange as GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Endpoint exposing (groups, groupsEnrollment, unwrap)
import Api.Helper exposing (get, put, delete, post)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


groupsGet : Int -> Int -> (WebData Group -> msg) -> Cmd msg
groupsGet courseId id msg =
    get (unwrap <| groups courseId id)
        msg
        Group.decoder


groupsPut : Int -> Int -> Group -> (WebData String -> msg) -> Cmd msg
groupsPut courseId id group msg =
    put (unwrap <| groups courseId id)
        (Http.jsonBody (Group.encoder group))
        msg
        Decode.string


groupsDelete : Int -> Int -> (WebData String -> msg) -> Cmd msg
groupsDelete courseId id msg =
    delete (unwrap <| groups courseId id)
        msg
        Decode.string


groupsEnrollmentPut : Int -> Int -> GroupEnrollmentChange -> (WebData String -> msg) -> Cmd msg
groupsEnrollmentPut courseId id groupEnrollment msg =
    put (unwrap <| groupsEnrollment courseId id)
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg
        Decode.string


groupsEnrollmentPost : Int -> Int -> GroupEnrollmentChange -> (WebData Group -> msg) -> Cmd msg
groupsEnrollmentPost courseId id groupEnrollment msg =
    post (unwrap <| groupsEnrollment courseId id)
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg
        Group.decoder
