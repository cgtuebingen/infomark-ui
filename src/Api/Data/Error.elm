module Api.Data.Error exposing (Error, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Error =
    { code : String
    , message : String
    }


decoder : Decoder Error
decoder =
    Decode.succeed Error
        |> required "code" Decode.string
        |> required "message" Decode.string


encoder : Error -> Encode.Value
encoder model =
    Encode.object
        [ ( "code", Encode.string model.code )
        , ( "message", Encode.string model.message )
        ]
