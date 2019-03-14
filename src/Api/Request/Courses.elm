module Api.Request.Courses exposing
    ( courseDelete
    , courseGet
    , courseGroupsGet
    , courseGroupsPost
    , courseOwnGroupGet
    , coursePut
    , courseSheetsGet
    , courseSheetsPost
    , coursesEnrollmentDelete
    , coursesEnrollmentGet
    , coursesEnrollmentGetAll
    , coursesEnrollmentGetByEmail
    , coursesEnrollmentGetTeam
    , coursesEnrollmentPost
    , coursesGet
    , coursesPost
    , coursesBidsGet
    , coursesBidsPost
    )

import Api.Data.Course as Course exposing (Course)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupBid as GroupBid exposing (GroupBid)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Api.Data.Submission as Submission exposing (Submission)
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Endpoint
    exposing
        ( course
        , courseEnrollment
        , courseGroup
        , courseGroupBids
        , courseGroups
        , courseSheets
        , courses
        , submissions
        , unwrap
        )
import Api.Helper exposing (post, get, putExpectNothing, deleteExpectNothing, postExpectNothing)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder exposing (QueryParameter)


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


coursePut : Int -> Course -> (WebData () -> msg) -> Cmd msg
coursePut id courseUp msg =
    putExpectNothing (unwrap <| course id)
        (Http.jsonBody (Course.encoder courseUp))
        msg


courseDelete : Int -> (WebData () -> msg) -> Cmd msg
courseDelete id msg =
    deleteExpectNothing (unwrap <| course id)
        msg


coursesEnrollmentGet : Int -> List QueryParameter -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
coursesEnrollmentGet courseId params msg =
    get (unwrap <| courseEnrollment courseId params)
        msg
    <|
        Decode.list UserEnrollment.decoder


coursesEnrollmentGetAll : Int -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
coursesEnrollmentGetAll courseId msg =
    coursesEnrollmentGet courseId [] msg


coursesEnrollmentGetTeam : Int -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
coursesEnrollmentGetTeam courseId msg =
    let
        roles =
            String.join "," <|
                List.map String.fromInt <|
                    List.map CourseRole.roleToInt [ Admin, Tutor ]

        params =
            [ Url.Builder.string "roles" roles ]
    in
    coursesEnrollmentGet courseId params msg


coursesEnrollmentGetByEmail : Int -> String -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
coursesEnrollmentGetByEmail courseId emailToSearch msg =
    let
        params =
            [ Url.Builder.string "email" emailToSearch ]
    in
    coursesEnrollmentGet courseId params msg


coursesEnrollmentPost : Int -> (WebData () -> msg) -> Cmd msg
coursesEnrollmentPost courseId msg =
    postExpectNothing (unwrap <| courseEnrollment courseId [])
        Http.emptyBody
        msg


coursesEnrollmentDelete : Int -> (WebData () -> msg) -> Cmd msg
coursesEnrollmentDelete courseId msg =
    deleteExpectNothing (unwrap <| courseEnrollment courseId [])
        msg


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


courseSheetsGet : Int -> (WebData (List Sheet) -> msg) -> Cmd msg
courseSheetsGet id msg =
    get (unwrap <| courseSheets id)
        msg
    <|
        Decode.list Sheet.decoder


courseSheetsPost : Int -> Sheet -> (WebData Sheet -> msg) -> Cmd msg
courseSheetsPost id sheetNew msg =
    post (unwrap <| courseSheets id)
        (Http.jsonBody <| Sheet.encoder sheetNew)
        msg
        Sheet.decoder


coursesSubmissions : Int -> List QueryParameter -> (WebData (List Submission) -> msg) -> Cmd msg
coursesSubmissions courseId params msg =
    get (unwrap <| submissions courseId params)
        msg
    <|
        Decode.list Submission.decoder


submissionsForTaskPerGroup : Int -> Int -> Int -> (WebData (List Submission) -> msg) -> Cmd msg
submissionsForTaskPerGroup courseId taskId groupId msg =
    let
        params =
            [ Url.Builder.int "task_id" taskId
            , Url.Builder.int "group_id" groupId
            ]
    in
    coursesSubmissions courseId params msg


submissionsForUserPerSheet : Int -> Int -> Int -> (WebData (List Submission) -> msg) -> Cmd msg
submissionsForUserPerSheet courseId userId sheetId msg =
    let
        params =
            [ Url.Builder.int "user_id" userId
            , Url.Builder.int "sheet_id" sheetId
            ]
    in
    coursesSubmissions courseId params msg
