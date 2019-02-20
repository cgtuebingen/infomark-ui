module Api.Data.GroupBid exposing (GroupBid, decoder, encoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias GroupBid =
    { id : Int
    , groupId : Int
    , userId : Int
    , bid : Int
    }


decoder : Decoder GroupBid
decoder =
    Decode.succeed GroupBid
        |> required "id" Decode.int
        |> required "group_id" Decode.int
        |> required "user_id" Decode.int
        |> required "bid" Decode.int


encoder : GroupBid -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "group_id", Encode.int model.groupId )
        , ( "user_id", Encode.int model.userId )
        , ( "bid", Encode.int model.bid )
        ]
