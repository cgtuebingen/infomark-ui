module Api.Data.Sheet exposing (Sheet, decoder, encoder)

import Api.Data.Task as Task exposing (Task)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Time exposing (Posix)


type alias Sheet =
    { id : Int
    , name : String
    , file_url : Maybe String
    , published_at : Posix
    , due_at : Posix
    , tasks : Maybe (List Task)
    }


decoder : Decoder Sheet
decoder =
    Decode.succeed Sheet
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> optional "file_url" (Decode.nullable Decode.string) Nothing
        |> required "published_at" Iso8601.decoder
        |> required "due_at" Iso8601.decoder
        |> optional "tasks" (Decode.nullable <| Decode.list Task.decoder) Nothing


encoder : Sheet -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "file_url", maybe Encode.string model.file_url )
        , ( "published_at", Iso8601.encode model.published_at )
        , ( "due_at", Iso8601.encode model.due_at )
        , ( "tasks", maybe (Encode.list Task.encoder) model.tasks )
        ]
