module Api.Data.Account exposing (Account, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Account =
    { email : String
    , plain_password : String
    }


decoder : Decoder Account
decoder =
    Decode.succeed Account
        |> required "email" Decode.string
        |> required "plain_password" Decode.string


encoder : Account -> Encode.Value
encoder model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "plain_password", Encode.string model.plain_password )
        ]
