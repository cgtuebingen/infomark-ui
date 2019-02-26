module Api.Data.MailConfirmation exposing (MailConfirmation, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias MailConfirmation =
    { email : String
    , confirmationToken : String
    }


decoder : Decoder MailConfirmation
decoder =
    Decode.succeed MailConfirmation
        |> required "email" Decode.string
        |> required "confirmation_token" Decode.string


encoder : MailConfirmation -> Encode.Value
encoder model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "confirmation_token", Encode.string model.confirmationToken )
        ]
