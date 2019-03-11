module Api.Data.GroupEnrollmentChange exposing (GroupEnrollmentChange, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias GroupEnrollmentChange =
    { userId : Int }


decoder : Decoder GroupEnrollmentChange
decoder =
    Decode.succeed GroupEnrollmentChange
        |> required "user_id" Decode.int


encoder : GroupEnrollmentChange -> Encode.Value
encoder model =
    Encode.object
        [ ( "user_id", Encode.int model.userId ) ]
