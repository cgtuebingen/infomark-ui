git module Api.Request.Auth exposing (sessionDelete, sessionPost)

--import Api.Helper exposing (..)

import Api.Data.Account as Account exposing (Account)
import Api.Data.Error as Error exposing (Error)
import Api.Data.Role as Role exposing (Role)
import Api.Endpoint exposing (sessions, unwrap)
import Api.HelperMock exposing (..)
import Decoders
import Dict
import Http
import Http.Mock
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


mockSessionPost =
    let
        default_role =
            { root = True }
    in
    Http.GoodStatus_
        { url = "localhost:3000"
        , statusCode = 200
        , statusText = "Ok"
        , headers = Dict.fromList [ ( "Set-Cookie", "session=123456" ) ]
        }
        (Encode.encode 0 <| Role.encoder default_role)


sessionPost : Account -> (WebData Role -> msg) -> Cmd msg
sessionPost account msg =
    post (unwrap sessions)
        (Http.jsonBody (Account.encoder account))
        msg
        Role.decoder
        mockSessionPost



-- TODO Delete when we are not mocking anymore


mockSessionDelete =
    Http.GoodStatus_
        { url = "localhost:3000"
        , statusCode = 200
        , statusText = "Ok"
        , headers = Dict.fromList [ ( "Set-Cookie", "session=123456" ) ]
        }
        (Encode.encode
            0
            (Encode.string "")
        )


sessionDelete : (WebData String -> msg) -> Cmd msg
sessionDelete msg =
    delete (unwrap sessions)
        msg
        Decode.string
        mockSessionDelete



-- TODO Delete when we are not mocking anymore
