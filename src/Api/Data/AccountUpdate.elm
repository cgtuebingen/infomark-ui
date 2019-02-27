module Api.Data.AccountUpdate exposing (AccountUpdate, decoder, encoder)

import Api.Data.Account as Account exposing (Account)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias AccountUpdate =
    { account : Account
    , oldPassword : String
    }


decoder : Decoder AccountUpdate
decoder =
    Decode.succeed AccountUpdate
        |> required "account" Account.decoder
        |> required "old_plain_password" Decode.string


encoder : AccountUpdate -> Encode.Value
encoder model =
    Encode.object
        [ ( "account", Account.encoder model.account )
        , ( "old_plain_password", Encode.string model.oldPassword )
        ]
