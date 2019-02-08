module Api.Data.Role exposing (Role, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
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