module Api.Data.GroupSummary exposing (GroupSummary, decoder)

import Api.Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Sheet =
    { id : Int
    , name : String
    }


type alias UserInfo =
    { id : Int
    , first_name : String
    , last_name : String
    , studentNumber : Maybe String -- not seen by tutors
    , email : String
    }


type alias Achievement =
    { user_info : UserInfo
    , points : List Int
    }


type alias GroupSummary =
    { sheets : List Sheet
    , achievements : List Achievement
    }


sheetDecoder : Decoder Sheet
sheetDecoder =
    Decode.succeed Sheet
        |> required "id" Decode.int
        |> required "name" Decode.string


userInfoDecoder : Decoder UserInfo
userInfoDecoder =
    Decode.succeed UserInfo
        |> required "id" Decode.int
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> optional "student_number" (Decode.nullable Decode.string) Nothing
        |> required "email" Decode.string


achievementDecoder : Decoder Achievement
achievementDecoder =
    Decode.succeed Achievement
        |> required "user_info" userInfoDecoder
        |> required "points" (Decode.list Decode.int)


decoder : Decoder GroupSummary
decoder =
    Decode.succeed GroupSummary
        |> required "sheets" (Decode.list sheetDecoder)
        |> required "achievements" (Decode.list achievementDecoder)
