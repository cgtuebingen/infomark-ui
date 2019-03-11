module Api.Data.AccountEnrollment exposing (AccountEnrollment, decoder, encoder)

import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias AccountEnrollment =
    { id : Int
    , course_id : Int
    , role : CourseRole
    }


decoder : Decoder AccountEnrollment
decoder =
    Decode.succeed AccountEnrollment
        |> required "id" Decode.int
        |> required "course_id" Decode.int
        |> required "role" CourseRole.decoder


encoder : AccountEnrollment -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "course_id", Encode.int model.course_id )
        , ( "role", CourseRole.encoder model.role )
        ]
