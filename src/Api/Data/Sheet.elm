module Api.Data.Sheet exposing (Sheet, decoder, encoder)

import Api.Data.Task as Task exposing (Task)
import Dict exposing (Dict)
import Iso8061
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time


type alias Sheet =
    { id : Int
    , name : String
    , file_url : Maybe String
    , published_at : Time
    , due_at : Time
    , tasks : Maybe (List Task)
    }


decoder : Decoder Sheet
decoder =
    Decode.succeed Sheet
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> optional "file_url" (Decode.nullable Decoder.string) Nothing
        |> required "published_at" Iso8061.toTime
        |> required "due_at" Iso8061.toTime
        |> required "tasks" (Decoder.nullable <| Decode.list Task.decoder) Nothing


encoder : Sheet -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "file_url", Maybe.withDefault Encode.null <| Maybe.map Encode.string model.file_url )
        , ( "published_at", Iso8601.fromTime model.published_at )
        , ( "due_at", Iso8601.fromTime model.due_at )
        , ( "tasks", Maybe.withDefault Encode.null <| Maybe.map Encode.list Task.encoder model.tasks )
        ]
