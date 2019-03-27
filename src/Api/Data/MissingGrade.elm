module Api.Data.MissingGrade exposing (MissingGrade, decoder, encoder)

import Api.Data.Grade as Grade exposing (Grade)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias MissingGrade =
    { grade : Grade
    , course_id : Int
    , sheet_id : Int
    , task_id : Int
    }


decoder : Decoder MissingGrade
decoder =
    Decode.succeed MissingGrade
        |> required "grade" Grade.decoder
        |> required "course_id" Decode.int
        |> required "sheet_id" Decode.int
        |> required "task_id" Decode.int


encoder : MissingGrade -> Encode.Value
encoder model =
    Encode.object
        [ ( "grade", Grade.encoder model.grade )
        , ( "course_id", Encode.int model.course_id )
        , ( "sheet_id", Encode.int model.sheet_id )
        , ( "task_id", Encode.int model.task_id )
        ]
