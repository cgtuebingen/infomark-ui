module Api.Data.UserEnrollment exposing (UserEnrollment, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.User as User exposing (User)

type alias UserEnrollment =
    { role : CourseRole
    , user : User
    }


decoder : Decoder UserEnrollment
decoder =
    Decode.succeed UserEnrollment
        |> required "role" CourseRole.decoder
        |> required "user" User.decoder


encoder : UserEnrollment -> Encode.Value
encoder model =
    Encode.object
        [ ( "role", CourseRole.encoder model.role )
        , ( "user", User.encoder model.user)
        ]