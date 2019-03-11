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


taskGet : Int -> (WebData Task -> msg) -> Cmd msg
taskGet id msg =
    get (unwrap <| task id)
        msg
        Task.decoder


taskPut : Int -> Task -> (WebData () -> msg) -> Cmd msg
taskPut id taskUpdate msg =
    patchExpectNothing (unwrap <| task id)
        (Http.jsonBody <| Task.encoder taskUpdate)
        msg


taskPublicFilesPost : Int -> File -> (WebData () -> msg) -> Cmd msg
taskPublicFilesPost id file msg =
    postFile (unwrap <| taskPublicFiles id) file msg


taskPrivateFilesPost : Int -> File -> (WebData () -> msg) -> Cmd msg
taskPrivateFilesPost id file msg =
    postFile (unwrap <| taskPrivateFiles id) file msg


taskResultGet : Int -> (WebData Grade -> msg) -> Cmd msg
taskResultGet id msg =
    get (unwrap <| taskResult id)
        msg
        Grade.decoder
