module Api.Data.Role exposing (Role, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Role =
    { root : Bool
    }


decoder : Decoder Role
decoder =
    Decode.succeed Role
        |> required "root" Decode.bool


encoder : Role -> Encode.Value
encoder model =
    Encode.object
        [ ( "root", Encode.bool model.root )
        ]
