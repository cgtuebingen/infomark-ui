module Api.Request.Material exposing
    ( materialDelete
    , materialFilePost
    , materialGet
    , materialPut
    )

import Api.Data.Material as Material exposing (Material)
import Api.Endpoint exposing (material, materialFile, unwrap)
import Api.Helper exposing (deleteExpectNothing, get, post, postFile, putExpectNothing)
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


materialGet : Int -> Int -> (WebData Material -> msg) -> Cmd msg
materialGet courseId id msg =
    get (unwrap <| material courseId id) msg Material.decoder


materialPut : Int -> Int -> Material -> (WebData () -> msg) -> Cmd msg
materialPut courseId id materialUpdate msg =
    putExpectNothing (unwrap <| material courseId id)
        (Http.jsonBody <| Material.encoder materialUpdate)
        msg


materialDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
materialDelete courseId id msg =
    deleteExpectNothing (unwrap <| material courseId id) msg


materialFilePost : Int -> Int -> File -> (WebData () -> msg) -> Cmd msg
materialFilePost courseId id file msg =
    postFile (unwrap <| materialFile courseId id) file msg
