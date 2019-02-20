module Api.Data.Group exposing (Group, decoder, encoder)

import Api.Data.User as User exposing (User)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Group =
    { id : Int
    , description : String
    , tutor : User
    , courseId : Int
    , participants : Maybe (List User)
    }


decoder : Decoder Group
decoder =
    Decode.succeed Group
        |> required "id" Decode.int
        |> required "description" Decode.string
        |> required "tutor" User.decoder
        |> required "course_id" Decode.int
        |> optional "participants" (Decode.nullable (Decode.list User.decoder)) Nothing


encoder : Group -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "description", Encode.string model.description )
        , ( "tutor", User.encoder model.tutor )
        , ( "course_id", Encode.int model.courseId )
        , ( "participants", maybe (Encode.list User.encoder) model.participants )
        ]
