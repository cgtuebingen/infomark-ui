module Api.Data.Mail exposing
    ( Mail
    , decoder
    , encoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Mail =
    { subject : String
    , message : String
    }


decoder : Decoder Mail
decoder =
    Decode.succeed Mail
        |> required "subject" Decode.string
        |> required "body" Decode.string


encoder : Mail -> Encode.Value
encoder model =
    Encode.object
        [ ( "subject", Encode.string model.subject )
        , ( "body", Encode.string model.message )
        ]
