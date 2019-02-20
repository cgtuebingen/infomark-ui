module Api.Request.Account exposing (accountGet, accountEnrollmentGet)

import Api.Data.Error as Error exposing (Error)
import Api.Data.User as User exposing (User)
import Api.Data.AccountEnrollment as AccountEnrollment exposing (AccountEnrollment)
import Api.Endpoint exposing (account, accountEnrollment, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


accountGet : (WebData User -> msg) -> Cmd msg
accountGet msg =
    get (unwrap account) msg User.decoder


accountEnrollmentGet : (WebData (List AccountEnrollment) -> msg) -> Cmd msg
accountEnrollmentGet msg =
    get (unwrap accountEnrollment) msg <|
        Decode.list AccountEnrollment.decoder