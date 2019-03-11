module Api.Data.UpdatePassword exposing (UpdatePassword, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias UpdatePassword =
    { email : String
    , resetPasswordToken : String
    , password : String
    }


decoder : Decoder UpdatePassword
decoder =
    Decode.succeed UpdatePassword
        |> required "email" Decode.string
        |> required "reset_password_token" Decode.string
        |> required "plain_password" Decode.string


encoder : UpdatePassword -> Encode.Value
encoder model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "reset_password_token", Encode.string model.resetPasswordToken )
        , ( "plain_password", Encode.string model.password )
        ]
