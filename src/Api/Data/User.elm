module Api.Data.User exposing (User, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias User =
    { id : Int
    , firstname : String
    , lastname : String
    , avatarUrl : Maybe String
    , email : String
    , studentNumber : Maybe String
    , semester : Maybe Int
    , subject : Maybe String
    , language : Maybe String
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "id" Decode.int
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> optional "avatar_url" (Decode.nullable Decode.string) Nothing
        |> required "email" Decode.string
        |> optional "student_number" (Decode.nullable Decode.string) Nothing
        |> optional "semester" (Decode.nullable Decode.int) Nothing
        |> optional "subject" (Decode.nullable Decode.string) Nothing
        |> optional "language" (Decode.nullable Decode.string) Nothing


encoder : User -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "first_name", Encode.string model.firstname )
        , ( "last_name", Encode.string model.lastname )
        , ( "avatar_url", maybe Encode.string model.avatarUrl )
        , ( "email", Encode.string model.email )
        , ( "student_number", maybe Encode.string model.studentNumber )
        , ( "semester", maybe Encode.int model.semester )
        , ( "subject", maybe Encode.string model.subject )
        , ( "language", maybe Encode.string model.language )
        ]
