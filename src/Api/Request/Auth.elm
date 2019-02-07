module Api.Request.Auth exposing (sessionPost, sessionDelete)

import Api.Data.Account as Account exposing (Account)
import Api.Data.Role as Role exposing (Role)
import Api.Data.Error as Error exposing (Error)
import Decoders
import Dict
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode as Decode
import Api.Endpoint exposing (sessions, unwrap)
import Api.Helper exposing (..)


sessionPost : Account -> (WebData Role -> msg) -> Cmd msg
sessionPost account msg = post (unwrap sessions) 
    (Http.jsonBody (Account.encoder account)) 
    msg Role.decoder

sessionDelete : (WebData Account -> msg) -> Cmd msg
sessionDelete msg = delete (unwrap sessions) msg Account.decoder
