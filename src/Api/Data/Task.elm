module Api.Data.Task exposing (Task, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Task =
    { id : Int
    , max_point : Int
    , public_tests_url : Maybe String
    , public_docker_image : Maybe String
    , private_tests_url : Maybe String
    , private_docker_image : Maybe String
    }


decoder : Decoder Task
decoder =
    Decode.succeed Task
        |> required "id" Decode.int
        |> required "max_point" Decode.int
        |> optional "public_tests_url" (Decode.nullable Decode.string) Nothing
        |> optional "public_docker_image" (Decode.nullable Decode.string) Nothing
        |> optional "private_tests_url" (Decode.nullable Decode.string) Nothing
        |> optional "private_docker_image" (Decode.nullable Decode.string) Nothing


encoder : Task -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "max_point", Encode.int model.max_point )
        , ( "public_tests_url", Maybe.withDefault Encode.null (Maybe.map Encode.string model.public_tests_url) )
        , ( "public_docker_image", Maybe.withDefault Encode.null (Maybe.map Encode.string model.public_docker_image) )
        , ( "private_tests_url", Maybe.withDefault Encode.null (Maybe.map Encode.string model.private_tests_url) )
        , ( "private_docker_image", Maybe.withDefault Encode.null (Maybe.map Encode.string model.private_docker_image) )
        ]
