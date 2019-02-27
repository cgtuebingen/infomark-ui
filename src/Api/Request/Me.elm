module Api.Request.Me exposing
    ( meGet
    , mePut
    )

import Api.Data.Error as Error exposing (Error)
import Api.Data.User as User exposing (User)
import Api.Endpoint exposing (me, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


meGet : (WebData User -> msg) -> Cmd msg
meGet msg =
    get (unwrap me) msg User.decoder


mePut : User -> (WebData () -> msg) -> Cmd msg
mePut userUpdate msg =
    putExpectNothing (unwrap me)
        (Http.jsonBody <| User.encoder userUpdate)
        msg
