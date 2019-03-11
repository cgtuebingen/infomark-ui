module Api.Data.Grade exposing (Grade, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Grade =
    { id : Int
    , public_execution_state : Int
    , private_execution_state : Int
    , public_test_log : String
    , private_test_log : String
    , public_test_status : Int
    , private_test_status : Int
    , acquired_points : Int
    , feedback : String
    , tutor_id : Int
    , submission_id : Int
    }


decoder : Decoder Grade
decoder =
    Decode.succeed Grade
        |> required "id" Decode.int
        |> required "public_execution_state" Decode.int
        |> required "private_execution_state" Decode.int
        |> required "public_test_log" Decode.string
        |> required "private_test_log" Decode.string
        |> required "public_test_status" Decode.int
        |> required "private_test_status" Decode.int
        |> required "acquired_points" Decode.int
        |> required "feedback" Decode.string
        |> required "tutor_id" Decode.int
        |> required "submission_id" Decode.string

encoder : Grade -> Encode.Value
encoder model = 
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "public_execution_state", Encode.int model.public_execution_state )
        , ( "private_execution_state", Encode.int model.private_execution_state )
        , ( "public_test_log", Encode.string model.public_test_log )
        , ( "private_test_log", Encode.string model.private_test_log )
        , ( "public_test_status", Encode.int model.public_test_status )
        , ( "private_test_status", Encode.int model.private_test_status )
        , ( "acquired_points", Encode.int model.acquired_points )
        , ( "feedback", Encode.string model.feedback )
        , ( "tutor_id", Encode.int model.public_execution_state )
        , ( "submission_id", Encode.int model.submission_id )
        ]