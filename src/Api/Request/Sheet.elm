module Api.Request.Sheet exposing
    ( sheetDelete
    , sheetGet
    , sheetPut
    , sheetFilePost
    , sheetTasksGet
    , sheetTasksPost
    )

import Api.Data.Error as Error exposing (Error)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Api.Data.Task as Task exposing (Task)
import Api.Endpoint exposing (sheet, sheetFile, sheetTasks, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


sheetGet : Int -> (WebData Sheet -> msg) -> Cmd msg
sheetGet id msg =
    get (unwrap <| sheet id) msg Sheet.decoder


sheetPut : Int -> Sheet -> (WebData () -> msg) -> Cmd msg
sheetPut id sheetUpdate msg =
    putExpectNothing (unwrap <| sheet id)
        (Http.jsonBody <| Sheet.encoder sheetUpdate)
        msg


sheetDelete : Int -> (WebData () -> msg) -> Cmd msg
sheetDelete id msg =
    deleteExpectNothing (unwrap <| sheet id) msg


sheetFilePost : Int -> File -> (WebData () -> msg) -> Cmd msg
sheetFilePost id file msg =
    postFile (unwrap <| sheetFile id) file msg


sheetTasksPost : Int -> Task -> (WebData Task -> msg) -> Cmd msg
sheetTasksPost id taskNew msg =
    post (unwrap <| sheetTasks id)
        (Http.jsonBody <| Task.encoder taskNew)
        msg
        Task.decoder


sheetTasksGet : Int -> (WebData (List Task) -> msg) -> Cmd msg
sheetTasksGet id msg =
    get (unwrap <| sheetTasks id)
        msg
    <|
        Decode.list Task.decoder