module Api.Request.Groups exposing
    ( groupsDelete
    , groupsEnrollmentGetAll
    , groupsEnrollmentPost
    , groupsEnrollmentPut
    , groupsGet
    , groupsPut
    )

import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupEnrollmentChange as GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Endpoint exposing (group, groupsEnrollment, unwrap)
import Api.Helper exposing (delete, deleteExpectNothing, get, post, postExpectNothing, put, putExpectNothing)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


groupsGet : Int -> Int -> (WebData Group -> msg) -> Cmd msg
groupsGet courseId id msg =
    get (unwrap <| group courseId id)
        msg
        Group.decoder


groupsPut : Int -> Int -> Group -> (WebData () -> msg) -> Cmd msg
groupsPut courseId id groupUpdate msg =
    putExpectNothing (unwrap <| group courseId id)
        (Http.jsonBody (Group.encoder groupUpdate))
        msg


groupsDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
groupsDelete courseId id msg =
    deleteExpectNothing (unwrap <| group courseId id)
        msg


groupsEnrollmentPut : Int -> Int -> GroupEnrollmentChange -> (WebData () -> msg) -> Cmd msg
groupsEnrollmentPut courseId id groupEnrollment msg =
    putExpectNothing (unwrap <| groupsEnrollment courseId id [])
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg


groupsEnrollmentPost : Int -> Int -> GroupEnrollmentChange -> (WebData () -> msg) -> Cmd msg
groupsEnrollmentPost courseId id groupEnrollment msg =
    postExpectNothing (unwrap <| groupsEnrollment courseId id [])
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg


groupsEnrollmentGetAll : Int -> Int -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
groupsEnrollmentGetAll courseId groupId msg =
    get (unwrap <| groupsEnrollment courseId groupId [])
        msg
    <|
        Decode.list UserEnrollment.decoder
