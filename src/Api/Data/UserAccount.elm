module Api.Data.UserAccount exposing (UserAccount, decoder, encoder)

import Api.Data.Account as Account exposing (Account)
import Api.Data.User as User exposing (User)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias UserAccount =
    { user : Maybe User
    , account : Maybe Account
    }


decoder : Decoder UserAccount
decoder =
    Decode.succeed UserAccount
        |> optional "user" (Decode.nullable User.decoder) Nothing
        |> optional "account" (Decode.nullable Account.decoder) Nothing


encoder : UserAccount -> Encode.Value
encoder model =
    Encode.object
        [ ( "user", Maybe.withDefault Encode.null (Maybe.map User.encoder model.user) )
        , ( "account", Maybe.withDefault Encode.null (Maybe.map Account.encoder model.account) )
        ]
