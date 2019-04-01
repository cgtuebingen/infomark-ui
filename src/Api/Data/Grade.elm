module Api.Data.Grade exposing (ExecutionState(..), Grade, TestStatus(..), decoder, encoder)

import Api.Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Grade =
    { id : Int
    , public_execution_state : ExecutionState
    , private_execution_state : ExecutionState
    , public_test_log : String
    , private_test_log : String
    , public_test_status : TestStatus
    , private_test_status : TestStatus
    , acquired_points : Int
    , feedback : String
    , tutor_id : Int
    , submission_id : Int
    , user : User
    , file_url : Maybe String
    }


decoder : Decoder Grade
decoder =
    Decode.succeed Grade
        |> required "id" Decode.int
        |> required "public_execution_state" executionStateDecoder
        |> required "private_execution_state" executionStateDecoder
        |> required "public_test_log" Decode.string
        |> required "private_test_log" Decode.string
        |> required "public_test_status" testStatusDecoder
        |> required "private_test_status" testStatusDecoder
        |> required "acquired_points" Decode.int
        |> required "feedback" Decode.string
        |> required "tutor_id" Decode.int
        |> required "submission_id" Decode.int
        |> required "user" User.decoder
        |> optional "file_url" (Decode.nullable Decode.string) Nothing


encoder : Grade -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "public_execution_state", executionStateEncoder model.public_execution_state )
        , ( "private_execution_state", executionStateEncoder model.private_execution_state )
        , ( "public_test_log", Encode.string model.public_test_log )
        , ( "private_test_log", Encode.string model.private_test_log )
        , ( "public_test_status", testStatusEncoder model.public_test_status )
        , ( "private_test_status", testStatusEncoder model.private_test_status )
        , ( "acquired_points", Encode.int model.acquired_points )
        , ( "feedback", Encode.string model.feedback )
        , ( "tutor_id", Encode.int model.tutor_id )
        , ( "submission_id", Encode.int model.submission_id )
        , ( "user", User.encoder model.user )
        , ( "file_url", maybe Encode.string model.file_url )
        ]


type ExecutionState
    = Pending
    | Running
    | Finished


executionStateDecoder : Decoder ExecutionState
executionStateDecoder =
    Decode.int
        |> Decode.andThen
            (\v ->
                case v of
                    0 ->
                        Decode.succeed Pending

                    1 ->
                        Decode.succeed Running

                    _ ->
                        Decode.succeed Finished
            )


executionStateEncoder : ExecutionState -> Encode.Value
executionStateEncoder executionState =
    case executionState of
        Pending ->
            Encode.int 0

        Running ->
            Encode.int 1

        Finished ->
            Encode.int 2


type TestStatus
    = Ok
    | Failed


testStatusDecoder : Decoder TestStatus
testStatusDecoder =
    Decode.int
        |> Decode.andThen
            (\v ->
                case v of
                    0 ->
                        Decode.succeed Ok

                    _ ->
                        Decode.succeed Failed
            )


testStatusEncoder : TestStatus -> Encode.Value
testStatusEncoder testStatus =
    case testStatus of
        Ok ->
            Encode.int 0

        Failed ->
            Encode.int 1
