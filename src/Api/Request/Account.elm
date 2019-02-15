module Api.Request.Account exposing (accountGet)

import Api.Data.Error as Error exposing (Error)
import Api.Data.User as User exposing (User)
import Api.Endpoint exposing (account, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


accountGet : (WebData User -> msg) -> Cmd msg
accountGet msg =
    get (unwrap account) msg User.decoder
