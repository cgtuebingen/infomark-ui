module Api.Data.CourseRole exposing (CourseRole(..), decoder, encoder, intToRole, roleToInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type CourseRole
    = Student
    | Tutor
    | Admin


decoder : Decoder CourseRole
decoder =
    Decode.int
        |> Decode.andThen
            (\int ->
                case intToRole int of
                    Just role ->
                        Decode.succeed role

                    Nothing ->
                        Decode.fail "Not recognized role"
            )


encoder : CourseRole -> Encode.Value
encoder courseRole =
    Encode.int <| roleToInt courseRole


intToRole : Int -> Maybe CourseRole
intToRole int =
    case int of
        2 ->
            Just Admin

        1 ->
            Just Tutor

        0 ->
            Just Student

        _ ->
            Nothing


roleToInt : CourseRole -> Int
roleToInt role =
    case role of
        Student ->
            0

        Tutor ->
            1

        Admin ->
            2
