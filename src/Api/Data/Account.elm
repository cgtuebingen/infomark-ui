module Api.Data.Account exposing (Account, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Account =
    { email : Maybe String
    , plain_password : Maybe String
    }


decoder : Decoder Account
decoder =
    Decode.succeed Account
        |> optional "email" (Decode.nullable Decode.string) Nothing
        |> optional "plain_password" (Decode.nullable Decode.string) Nothing


encoder : Account -> Encode.Value
encoder model =
    Encode.object
        [ ( "email", maybe Encode.string model.email )
        , ( "plain_password", maybe Encode.string model.plain_password )
        ]
