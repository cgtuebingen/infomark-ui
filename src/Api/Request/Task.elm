module Api.Request.Task exposing
    ( taskGet
    , taskPrivateFilesPost
    , taskPublicFilesPost
    , taskPut
    , taskRatingGet
    , taskRatingPost
    , taskResultGet
    , taskSubmissionPost
    )

import Api.Data.Grade as Grade exposing (Grade)
import Api.Data.Task as Task exposing (Task)
import Api.Data.TaskRatingResponse as TaskRatingResponse exposing (TaskRatingResponse)
import Api.Endpoint exposing (task, taskPrivateFiles, taskPublicFiles, taskRating, taskResult, taskSubmission, unwrap)
import Api.Helper exposing (get, patchExpectNothing, postExpectNothing, postFile, putExpectNothing)
import File exposing (File)
import Http
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


taskGet : Int -> Int -> (WebData Task -> msg) -> Cmd msg
taskGet courseId id msg =
    get (unwrap <| task courseId id)
        msg
        Task.decoder


taskPut : Int -> Int -> Task -> (WebData () -> msg) -> Cmd msg
taskPut courseId id taskUpdate msg =
    putExpectNothing (unwrap <| task courseId id)
        (Http.jsonBody <| Task.encoder taskUpdate)
        msg


taskPublicFilesPost : Int -> Int -> File -> (WebData () -> msg) -> Cmd msg
taskPublicFilesPost courseId id file msg =
    postFile (unwrap <| taskPublicFiles courseId id) file msg


taskPrivateFilesPost : Int -> Int -> File -> (WebData () -> msg) -> Cmd msg
taskPrivateFilesPost courseId id file msg =
    postFile (unwrap <| taskPrivateFiles courseId id) file msg


taskResultGet : Int -> Int -> (WebData Grade -> msg) -> Cmd msg
taskResultGet courseId id msg =
    get (unwrap <| taskResult courseId id)
        msg
        Grade.decoder


taskRatingPost : Int -> Int -> Int -> (WebData () -> msg) -> Cmd msg
taskRatingPost courseId taskId rating msg =
    postExpectNothing (unwrap <| taskRating courseId taskId)
        (Http.jsonBody <| Encode.object [ ( "rating", Encode.int rating ) ])
        msg


taskRatingGet : Int -> Int -> (WebData TaskRatingResponse -> msg) -> Cmd msg
taskRatingGet courseId taskId msg =
    get (unwrap <| taskRating courseId taskId)
        msg
        TaskRatingResponse.decoder


taskSubmissionPost : Int -> Int -> File -> (WebData () -> msg) -> Cmd msg
taskSubmissionPost courseId taskId file msg =
    postFile (unwrap <| taskSubmission courseId taskId) file msg
