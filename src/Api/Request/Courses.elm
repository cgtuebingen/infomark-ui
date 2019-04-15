module Api.Request.Courses exposing
    ( courseDelete
    , courseGet
    , courseGradeGet
    , courseGradeMissing
    , courseGradePut
    , courseGradesGetPerTaskAndGroup
    , courseGroupsGet
    , courseGroupsPost
    , courseMaterialsGet
    , courseMaterialsPost
    , courseOwnGroupGet
    , coursePointsGet
    , coursePut
    , courseSheetsGet
    , courseSheetsPost
    , courseTaskMissing
    , coursesBidsGet
    , coursesBidsPost
    , coursesEnrollmentDelete
    , coursesEnrollmentGet
    , coursesEnrollmentGetAll
    , coursesEnrollmentGetByEmail
    , coursesEnrollmentGetByQuery
    , coursesEnrollmentGetTeam
    , coursesEnrollmentPost
    , coursesEnrollmentsUserDelete
    , coursesEnrollmentsUserGet
    , coursesEnrollmentsUserPut
    , coursesGet
    , coursesPost
    )

import Api.Data.Course as Course exposing (Course)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.Grade as Grade exposing (Grade)
import Api.Data.Group as Group exposing (Group)
import Api.Data.GroupBid as GroupBid exposing (GroupBid)
import Api.Data.Material as Material exposing (Material)
import Api.Data.MissingGrade as MissingGrade exposing (MissingGrade)
import Api.Data.MissingTask as MissingTask exposing (MissingTask)
import Api.Data.PointOverview as PointOverview exposing (PointOverview)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Api.Data.Submission as Submission exposing (Submission)
import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Endpoint
    exposing
        ( course
        , courseBids
        , courseEnrollment
        , courseEnrollmentUserDetail
        , courseGrade
        , courseGrades
        , courseGroupBid
        , courseGroups
        , courseGroupsOwn
        , courseMaterials
        , courseMissingGrades
        , courseMissingTasks
        , coursePoints
        , courseSheets
        , courses
        , submissions
        , unwrap
        )
import Api.Helper
    exposing
        ( deleteExpectNothing
        , get
        , post
        , postExpectNothing
        , putExpectNothing
        )
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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


coursesEnrollmentGetByQuery : Int -> String -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
coursesEnrollmentGetByQuery courseId query msg =
    let
        params =
            [ Url.Builder.string "q" query ]
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


coursesEnrollmentsUserGet : Int -> Int -> (WebData UserEnrollment -> msg) -> Cmd msg
coursesEnrollmentsUserGet courseId userId msg =
    get (unwrap <| courseEnrollmentUserDetail courseId userId)
        msg
        UserEnrollment.decoder


coursesEnrollmentsUserPut : Int -> Int -> CourseRole -> (WebData () -> msg) -> Cmd msg
coursesEnrollmentsUserPut courseId userId newRole msg =
    putExpectNothing (unwrap <| courseEnrollmentUserDetail courseId userId)
        (Encode.object
            [ ( "role", CourseRole.encoder newRole )
            ]
            |> Http.jsonBody
        )
        msg


coursesEnrollmentsUserDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
coursesEnrollmentsUserDelete courseId userId msg =
    deleteExpectNothing (unwrap <| courseEnrollmentUserDetail courseId userId)
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


courseOwnGroupGet : Int -> (WebData (List Group) -> msg) -> Cmd msg
courseOwnGroupGet id msg =
    get (unwrap <| courseGroupsOwn id)
        msg
    <|
        Decode.list Group.decoder


coursesBidsGet : Int -> (WebData (List GroupBid) -> msg) -> Cmd msg
coursesBidsGet id msg =
    get (unwrap <| courseBids id)
        msg
    <|
        Decode.list GroupBid.decoder


coursesBidsPost : Int -> Int -> Int -> (WebData () -> msg) -> Cmd msg
coursesBidsPost courseId groupId bid msg =
    postExpectNothing (unwrap <| courseGroupBid courseId groupId)
        (Encode.object
            [ ( "bid", Encode.int bid )
            ]
            |> Http.jsonBody
        )
        msg


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


courseMaterialsGet : Int -> (WebData (List Material) -> msg) -> Cmd msg
courseMaterialsGet id msg =
    get (unwrap <| courseMaterials id)
        msg
    <|
        Decode.list Material.decoder


courseMaterialsPost : Int -> Material -> (WebData Material -> msg) -> Cmd msg
courseMaterialsPost id materialNew msg =
    post (unwrap <| courseMaterials id)
        (Http.jsonBody <| Material.encoder materialNew)
        msg
        Material.decoder


coursePointsGet : Int -> (WebData (List PointOverview) -> msg) -> Cmd msg
coursePointsGet id msg =
    get (unwrap <| coursePoints id)
        msg
    <|
        Decode.list PointOverview.decoder


courseGradesGet : Int -> List QueryParameter -> (WebData (List Grade) -> msg) -> Cmd msg
courseGradesGet courseId params msg =
    get (unwrap <| courseGrades courseId params)
        msg
    <|
        Decode.list Grade.decoder


courseGradesGetPerTaskAndGroup : Int -> Int -> Int -> (WebData (List Grade) -> msg) -> Cmd msg
courseGradesGetPerTaskAndGroup courseId taskId groupId msg =
    let
        params =
            [ Url.Builder.int "task_id" taskId
            , Url.Builder.int "group_id" groupId
            ]
    in
    courseGradesGet courseId params msg


courseGradeGet : Int -> Int -> (WebData Grade -> msg) -> Cmd msg
courseGradeGet courseId gradeId msg =
    get (unwrap <| courseGrade courseId gradeId) msg Grade.decoder


courseGradePut : Int -> Int -> Grade -> (WebData () -> msg) -> Cmd msg
courseGradePut courseId gradeId gradeUpdate msg =
    putExpectNothing (unwrap <| courseGrade courseId gradeId)
        (Http.jsonBody <| Grade.encoder gradeUpdate)
        msg


courseGradeMissing : Int -> (WebData (List MissingGrade) -> msg) -> Cmd msg
courseGradeMissing courseId msg =
    get (unwrap <| courseMissingGrades courseId)
        msg
    <|
        Decode.list MissingGrade.decoder


courseTaskMissing : Int -> (WebData (List MissingTask) -> msg) -> Cmd msg
courseTaskMissing courseId msg =
    get (unwrap <| courseMissingTasks courseId)
        msg
    <|
        Decode.list MissingTask.decoder
