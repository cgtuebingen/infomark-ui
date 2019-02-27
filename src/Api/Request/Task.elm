module Api.Request.Task exposing
    ( taskGet
    , taskPut
    , taskPublicFilesPost
    , taskPrivateFilesPost
    )

import Api.Data.Error as Error exposing (Error)
import Api.Data.Task as Task exposing (Task)
import Api.Endpoint exposing (task, taskPublicFiles, taskPrivateFiles, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


taskGet : Int -> (WebData Task -> msg) -> Cmd msg
taskGet id msg =
    get (unwrap <| task id) 
        msg 
        Task.decoder


taskPut : Int -> Task -> (WebData () -> msg) -> Cmd msg
taskPut id taskUpdate =
    patchExpectNothing (unwrap <| task id)
        (Http.jsonBody <| Task.encoder taskUpdate)
        msg


taskPublicFilesPost : Int -> File -> (WebData () -> msg) -> Cmd msg
taskPublicFilesPost id file msg =
    postFile (unwrap <| task id) file msg


taskPrivateFilesPost : Int -> File -> (WebData () -> msg) -> Cmd msg
taskPrivateFilesPost id file msg =
    postFile (unwrap <| task id) file msg
