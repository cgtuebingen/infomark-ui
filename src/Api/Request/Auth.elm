module Api.Request.Auth exposing (confirmMailPost, requestPasswordResetPost, sessionDelete, sessionPost, updatePasswordPost)

--import Api.Helper exposing (..)

import Api.Data.Account as Account exposing (Account)
import Api.Data.Error as Error exposing (Error)
import Api.Data.Role as Role exposing (Role)
import Api.Data.MailConfirmation as MailConfirmation exposing (MailConfirmation)
import Api.Data.UpdatePassword as UpdatePassword exposing (UpdatePassword)
import Api.Endpoint exposing (confirmEmail, updatePassword, requestPasswordReset, sessions, unwrap)
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

confirmMailPost : MailConfirmation -> (WebData () -> msg) -> Cmd msg
confirmMailPost confirmation msg =
    postExpectNothing (unwrap confirmEmail)
        (Http.jsonBody <| MailConfirmation.encoder confirmation)
        msg

requestPasswordResetPost : String -> (WebData () -> msg) -> Cmd msg
requestPasswordResetPost email msg =
    postExpectNothing (unwrap requestPasswordReset)
        ( Http.jsonBody <| 
            Encode.object [ ("email", Encode.string email) ] )
        msg

updatePasswordPost : UpdatePassword -> (WebData () -> msg) -> Cmd msg
updatePasswordPost updatePasswordObject msg =
    postExpectNothing (unwrap updatePassword)
        (Http.jsonBody <| UpdatePassword.encoder updatePasswordObject)
        msg