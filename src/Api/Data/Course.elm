module Api.Data.Course exposing (Course, decoder, encoder)

import Api.Data.Material as Material exposing (Material)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Time exposing (Posix)


type alias Course =
    { id : Int
    , name : String
    , description : Maybe String
    , begins_at : Posix
    , ends_at : Posix
    , required_percentage : Maybe Int
    , sheets : Maybe (List Sheet)
    , materials : Maybe (List Material)
    }

decoder : Decoder Course
decoder =
    Decode.succeed Course
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> optional "description" (Decode.nullable Decode.string) Nothing
        |> required "begins_at" Iso8601.decoder
        |> required "ends_at" Iso8601.decoder
        |> optional "required_points" (Decode.nullable Decode.int) Nothing
        |> optional "sheets" (Decode.nullable <| Decode.list Sheet.decoder) Nothing
        |> optional "materials" (Decode.nullable <| Decode.list Material.decoder) Nothing


encoder : Course -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "description", maybe Encode.string model.description )
        , ( "begins_at", Iso8601.encode model.begins_at )
        , ( "ends_at", Iso8601.encode model.ends_at )
        , ( "required_points", maybe Encode.int model.required_percentage )
        , ( "sheets", maybe (Encode.list Sheet.encoder) model.sheets )
        , ( "materials", maybe (Encode.list Material.encoder) model.materials )
        ]
