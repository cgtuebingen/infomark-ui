module Api.Data.GroupOverview exposing (GroupOverview, decoder)

import Api.Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias GroupOverview =
    { user_id : Int
    , sheet_id : Int
    , sheet_name : String
    , points : Int
    }


decoder : Decoder GroupOverview
decoder =
    Decode.succeed GroupOverview
        |> required "user_id" Decode.int
        |> required "sheet_id" Decode.int
        |> required "name" Decode.string
        |> required "points" Decode.int
