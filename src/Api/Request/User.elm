module Api.Request.User exposing
    ( usersGet
    , userGet
    , userPatch
    )

import Api.Data.Error as Error exposing (Error)
import Api.Data.User as User exposing (User)
import Api.Endpoint exposing (user, users, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


usersGet : (WebData (List User) -> msg) -> Cmd msg
usersGet msg =
    get (unwrap users) msg <|
        Decode.list User.decoder


userGet : Int -> (WebData User -> msg) -> Cmd msg
userGet id msg =
    get (unwrap (user id)) msg User.decoder


userPatch : Int -> User -> (WebData () -> msg) -> Cmd msg
userPatch id userUpdate msg =
    patchExpectNothing (unwrap (user id))
        (Http.jsonBody <| User.encoder userUpdate)
        msg
