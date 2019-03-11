module Api.Request.User exposing
    ( userGet
    , userPut
    , usersGet
    )

import Api.Data.User as User exposing (User)
import Api.Endpoint exposing (unwrap, user, users)
import Api.Helper exposing (get, patchExpectNothing)
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


userPut : Int -> User -> (WebData () -> msg) -> Cmd msg
userPut id userUpdate msg =
    patchExpectNothing (unwrap (user id))
        (Http.jsonBody <| User.encoder userUpdate)
        msg
