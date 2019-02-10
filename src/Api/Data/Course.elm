module Api.Data.Course exposing (Course)

import Api.Data.Material as Material exposing (Material)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time


type alias Course =
    { id : Int
    , name : String
    , description : Maybe String
    , begins_at : Time
    , ends_at : Time
    , required_points : Maybe Int
    , sheets : Maybe (List Sheet)
    , materials : Maybe (List Material)
    }


decoder : Decoder Course
decoder =
    Decode.succeed Course
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> optional "description" (Decode.nullable Decoder.string) Nothing
        |> required "begins_at" Iso8601.toTime
        |> required "ends_at" Iso8601.toTime
        |> optional "required_points" (Decode.nullable Decoder.int) Nothing
        |> optional "sheets" (Decoder.nullable <| Decode.list Sheet.decoder) Nothing
        |> optional "materials" (Decoder.nullable <| Decode.list Material.decoder) Nothing


encoder : Course -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "description", Maybe.withDefault Encode.null <| Maybe.map Encode.string model.description )
        , ( "begins_at", Iso8601.fromTime model.begins_at )
        , ( "ends_at", Iso8601.fromTime model.ends_at )
        , ( "required_points", Maybe.withDefault Encode.null <| Maybe.map Encode.int model.id )
        , ( "sheets", Maybe.withDefault Encode.null <| Maybe.map Encode.list Sheet.encoder model.sheets )
        , ( "materials", Maybe.withDefault Encode.null <| Maybe.map Encode.list Material.encoder model.materials )
        ]
