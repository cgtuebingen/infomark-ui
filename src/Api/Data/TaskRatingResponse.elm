module Api.Data.TaskRatingResponse exposing (TaskRatingResponse, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias TaskRatingResponse =
    { task_id : Int
    , average_rating : Float
    , own_rating : Int
    }


decoder : Decoder TaskRatingResponse
decoder =
    Decode.succeed TaskRatingResponse
        |> required "task_id" Decode.int
        |> required "average_rating" Decode.float
        |> required "own_rating" Decode.int


encoder : TaskRatingResponse -> Encode.Value
encoder model =
    Encode.object
        [ ( "task_id", Encode.int model.task_id )
        , ( "average_rating", Encode.float model.average_rating )
        , ( "own_rating", Encode.int model.own_rating )
        ]
