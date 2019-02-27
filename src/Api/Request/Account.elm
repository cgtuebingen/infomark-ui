module Api.Request.Account exposing
    ( accountAvatarPost
    , accountEnrollmentGet
    , accountPatch
    , accountPost
    )

import Api.Data.Account as Account exposing (Account)
import Api.Data.AccountUpdate as AccountUpdate exposing (AccountUpdate)
import Api.Data.AccountEnrollment as AccountEnrollment exposing (AccountEnrollment)
import Api.Data.Error as Error exposing (Error)
import Api.Data.User as User exposing (User)
import Api.Data.UserAccount as UserAccount exposing (UserAccount)
import Api.Endpoint exposing (account, accountAvatar, accountEnrollment, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


accountPost : UserAccount -> (WebData User -> msg) -> Cmd msg
accountPost accountNew msg =
    post (unwrap account)
        (Http.jsonBody <| UserAccount.encoder accountNew)
        msg
        User.decoder


accountPatch : AccountUpdate -> (WebData () -> msg) -> Cmd msg
accountPatch accountUpdate msg =
    patchExpectNothing (unwrap account)
        (Http.jsonBody <| AccountUpdate.encoder accountUpdate)
        msg


accountEnrollmentGet : (WebData (List AccountEnrollment) -> msg) -> Cmd msg
accountEnrollmentGet msg =
    get (unwrap accountEnrollment) msg <|
        Decode.list AccountEnrollment.decoder


accountAvatarPost : File -> (WebData () -> msg) -> Cmd msg
accountAvatarPost file msg =
    postFile (unwrap accountAvatar) file msg
