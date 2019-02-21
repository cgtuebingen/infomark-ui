module Api.Request.Auth exposing (sessionDelete, sessionPost)

--import Api.Helper exposing (..)

import Api.Data.Account as Account exposing (Account)
import Api.Data.Error as Error exposing (Error)
import Api.Data.Role as Role exposing (Role)
import Api.Endpoint exposing (sessions, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Http.Mock
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


sessionPost : Account -> (WebData Role -> msg) -> Cmd msg
sessionPost account msg =
    post (unwrap sessions)
        (Http.jsonBody (Account.encoder account))
        msg
        Role.decoder


sessionDelete : (WebData () -> msg) -> Cmd msg
sessionDelete msg =
    deleteExpectNothing (unwrap sessions)
        msg
        