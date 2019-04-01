module Api.Data.User exposing (User, decoder, encoder)

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
    , studentNumber : String
    , semester : Int
    , subject : String
    , language : String
    , root : Maybe Bool
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "id" Decode.int
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> optional "avatar_url" (Decode.nullable Decode.string) Nothing
        |> optional "email" Decode.string ""
        |> optional "student_number" Decode.string ""
        |> optional "semester" Decode.int 0
        |> optional "subject" Decode.string ""
        |> optional "language" Decode.string ""
        |> optional "root" (Decode.nullable Decode.bool) Nothing


encoder : User -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "first_name", Encode.string model.firstname )
        , ( "last_name", Encode.string model.lastname )
        , ( "avatar_url", maybe Encode.string model.avatarUrl )
        , ( "email", Encode.string model.email )
        , ( "student_number", Encode.string model.studentNumber )
        , ( "semester", Encode.int model.semester )
        , ( "subject", Encode.string model.subject )
        , ( "language", Encode.string model.language )
        , ( "root", maybe Encode.bool model.root )
        ]
