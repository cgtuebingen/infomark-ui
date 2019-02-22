module Api.Request.Account exposing 
    ( accountGet
    , accountPost
    , accountPatch
    , accountAvatarPost
    , accountEnrollmentGet
    )

import Api.Data.Error as Error exposing (Error)
import Api.Data.User as User exposing (User)
import Api.Data.Account as Account exposing (Account)
import Api.Data.UserAccount as UserAccount exposing (UserAccount)
import Api.Data.AccountEnrollment as AccountEnrollment exposing (AccountEnrollment)
import Api.Endpoint exposing (account, accountAvatar, accountEnrollment, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import File exposing (File)
import RemoteData exposing (RemoteData(..), WebData)


accountGet : (WebData User -> msg) -> Cmd msg
accountGet msg =
    get (unwrap account) msg User.decoder


accountPost : UserAccount -> (WebData User -> msg) -> Cmd msg
accountPost accountNew msg =
    post (unwrap account)
        (Http.jsonBody <| UserAccount.encoder accountNew)
        msg
        User.decoder


accountPatch : Account -> (WebData User -> msg) -> Cmd msg
accountPatch accountUpdate msg =
    patch (unwrap account)
        (Http.jsonBody <| Account.encoder accountUpdate)
        msg
        User.decoder


accountEnrollmentGet : (WebData (List AccountEnrollment) -> msg) -> Cmd msg
accountEnrollmentGet msg =
    get (unwrap accountEnrollment) msg <|
        Decode.list AccountEnrollment.decoder


accountAvatarPost : File -> (WebData () -> msg) -> Cmd msg
accountAvatarPost file msg =
    postImage (unwrap accountAvatar) file msg