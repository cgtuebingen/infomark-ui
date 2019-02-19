module Api.Data.AccountEnrollment exposing (AccountEnrollment, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))

type alias AccountEnrollment =
    { course_id : Int
    , role : CourseRole
    }


decoder : Decoder AccountEnrollment
decoder =
    Decode.succeed AccountEnrollment
        |> required "course_id" Decode.int
        |> required "role" CourseRole.decoder


encoder : AccountEnrollment -> Encode.Value
encoder model =
    Encode.object
        [ ( "course_id", Encode.int model.course_id )
        , ( "role", CourseRole.encoder model.role)
        ]