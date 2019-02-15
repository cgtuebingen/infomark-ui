module Api.Data.UserAccount exposing (UserAccount, decoder, encoder)

import Api.Data.Account as Account exposing (Account)
import Api.Data.User as User exposing (User)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias UserAccount =
    { user : User
    , account : Account
    }


decoder : Decoder UserAccount
decoder =
    Decode.succeed UserAccount
        |> required "user" User.decoder
        |> required "account" Account.decoder


encoder : UserAccount -> Encode.Value
encoder model =
    Encode.object
        [ ( "user", User.encoder model.user )
        , ( "account", Account.encoder model.account )
        ]
