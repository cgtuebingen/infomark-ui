module Api.Request.Groups exposing
    ( groupsDelete
    , groupsEnrollmentPost
    , groupsEnrollmentPut
    , groupsGet
    , groupsPut
    )

import Api.Data.Group as Group exposing (Group)
import Api.Data.User as User exposing (User)
import Api.Data.GroupEnrollmentChange as GroupEnrollmentChange exposing (GroupEnrollmentChange)
import Api.Endpoint exposing (groups, groupsEnrollment, unwrap)
import Api.Helper exposing (delete, get, post, put, putExpectNothing, deleteExpectNothing, postExpectNothing)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


groupsGet : Int -> Int -> (WebData Group -> msg) -> Cmd msg
groupsGet courseId id msg =
    get (unwrap <| groups courseId id)
        msg
        Group.decoder


groupsPut : Int -> Int -> Group -> (WebData () -> msg) -> Cmd msg
groupsPut courseId id group msg =
    putExpectNothing (unwrap <| groups courseId id)
        (Http.jsonBody (Group.encoder group))
        msg


groupsDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
groupsDelete courseId id msg =
    deleteExpectNothing (unwrap <| groups courseId id)
        msg


groupsEnrollmentPut : Int -> Int -> GroupEnrollmentChange -> (WebData () -> msg) -> Cmd msg
groupsEnrollmentPut courseId id groupEnrollment msg =
    putExpectNothing (unwrap <| groupsEnrollment courseId id)
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg


groupsEnrollmentPost : Int -> Int -> GroupEnrollmentChange -> (WebData () -> msg) -> Cmd msg
groupsEnrollmentPost courseId id groupEnrollment msg =
    postExpectNothing (unwrap <| groupsEnrollment courseId id)
        (Http.jsonBody (GroupEnrollmentChange.encoder groupEnrollment))
        msg


groupsEnrollmentGet : Int -> Int -> (WebData (List User) -> msg) -> Cmd msg
groupsEnrollmentGet courseId groupId msg =
    get (unwrap <| groupsEnrollment courseId groupId)
        msg <|
        Decode.list User.decoder