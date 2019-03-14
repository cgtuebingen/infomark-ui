module Api.Data.Submission exposing (Submission, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Submission =
    { id : Int
    , user_id : Int
    , task_id : Int
    , file_url : String
    }


decoder : Decoder Submission
decoder =
    Decode.succeed Submission
        |> required "id" Decode.int
        |> required "user_id" Decode.int
        |> required "task_id" Decode.int
        |> required "file_url" Decode.string


encoder : Submission -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "user_id", Encode.int model.user_id )
        , ( "task_id", Encode.int model.task_id )
        , ( "file_url", Encode.string model.file_url )
        ]