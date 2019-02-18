module Api.Data.AccountEnrollment exposing (AccountEnrollment, CourseRole(..), decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode

type CourseRole 
    = Student
    | Tutor
    | Admin

type alias AccountEnrollment =
    { course_id : Int
    , role : CourseRole
    }


decoder : Decoder AccountEnrollment
decoder =
    Decode.succeed AccountEnrollment
        |> required "course_id" Decode.int
        |> required "role" roleDecoder


encoder : AccountEnrollment -> Encode.Value
encoder model =
    Encode.object
        [ ( "course_id", Encode.int model.course_id )
        , ( "role", roleEncoder model.role)
        ]


roleDecoder : Decoder CourseRole
roleDecoder =
    Decode.int
        |> Decode.andThen
            (\int ->
                case int of
                    2 -> Decode.succeed Admin
                    1 -> Decode.succeed Tutor
                    0 -> Decode.succeed Student
                    _ -> Decode.fail "Not recognized type"
            )


roleEncoder : CourseRole -> Encode.Value
roleEncoder courseRole =
    case courseRole of
        Student -> Encode.int 0
        Tutor -> Encode.int 1
        Admin -> Encode.int 2