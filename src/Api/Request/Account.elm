module Api.Request.Account exposing (..)

import Api.Data.User as User exposing (User)
import Api.Data.Error as Error exposing (Error)
import Decoders
import Dict
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode as Decode
import Api.Endpoint exposing (account, unwrap)
import Api.Helper exposing (..)

accountGet : (WebData User -> msg) -> Cmd msg
accountGet msg = get (unwrap account) msg User.decoder

