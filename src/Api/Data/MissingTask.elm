module Api.Data.MissingTask exposing (MissingTask, decoder, encoder)

import Api.Data.Task as Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias MissingTask =
    { task : Task
    , course_id : Int
    , sheet_id : Int
    }


decoder : Decoder MissingTask
decoder =
    Decode.succeed MissingTask
        |> required "task" Task.decoder
        |> required "course_id" Decode.int
        |> required "sheet_id" Decode.int


encoder : MissingTask -> Encode.Value
encoder model =
    Encode.object
        [ ( "task", Task.encoder model.task )
        , ( "course_id", Encode.int model.course_id )
        , ( "sheet_id", Encode.int model.sheet_id )
        ]
