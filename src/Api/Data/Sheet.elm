module Api.Data.Sheet exposing (Sheet, decoder, encoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Time exposing (Posix)


type alias Sheet =
    { id : Int
    , name : String
    , publish_at : Posix
    , due_at : Posix
    }


decoder : Decoder Sheet
decoder =
    Decode.succeed Sheet
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "publish_at" Iso8601.decoder
        |> required "due_at" Iso8601.decoder


encoder : Sheet -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "publish_at", Iso8601.encode model.publish_at )
        , ( "due_at", Iso8601.encode model.due_at )
        ]
