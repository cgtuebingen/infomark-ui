module Api.Request.Task exposing
    ( taskGet
    , taskPrivateFilesPost
    , taskPublicFilesPost
    , taskPut
    , taskResultGet
    )

import Api.Data.Task as Task exposing (Task)
import Api.Data.Grade as Grade exposing (Grade)
import Api.Endpoint exposing (task, taskPrivateFiles, taskPublicFiles, taskResult, unwrap)
import Api.Helper exposing (get, patchExpectNothing, postFile)
import File exposing (File)
import Http
import RemoteData exposing (RemoteData(..), WebData)


taskGet : Int -> Int -> (WebData Task -> msg) -> Cmd msg
taskGet courseId id msg =
    get (unwrap <| task courseId id)
        msg
        Task.decoder


taskPut : Int -> Int -> Task -> (WebData () -> msg) -> Cmd msg
taskPut courseId id taskUpdate msg =
    patchExpectNothing (unwrap <| task courseId id)
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
