module Api.Data.Material exposing (Material, decoder, encoder)

import Dict exposing (Dict)
import Iso8061
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time


type MaterialType
    = Supplementary
    | Slide


type alias Material =
    { id : Int
    , file_url : Maybe String
    , name : String
    , material_type : MaterialType -- TODO enum? supplementary or slide
    , published_at : Time
    , lecture_at : Time
    }


decoder : Decoder Material
decoder =
    Decode.succeed Material
        |> required "id" Decode.int
        |> optional "file_url" (Decode.nullable Decode.string) Nothing
        |> required "name" Decode.string
        |> required "material_type" typeDecoder
        |> required "published_at" Iso8601.toTime
        |> required "lecture_at" Iso8601.toTime


encoder : Material -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "file_url", Maybe.withDefault Encode.null <| Maybe.map Encode.string model.file_url )
        , ( "name", Encode.string model.name )
        , ( "material_type", typeEncoder model.material_type )
        , ( "published_at", Iso8601.fromTime model.published_at )
        , ( "lecture_at", Iso8601.fromTime model.lecture_at )
        ]


typeDecoder : Decoder MaterialType
typeDecoder =
    Decoder.string
        |> Decode.andThen
            (\str ->
                case str of
                    "supplementary" ->
                        Decode.succeed Supplementary

                    "slide" ->
                        Decode.succeed Slide
            )


typeEncoder : MaterialType -> Encode.Value
typeEncoder materialType =
    case materialType of
        Supplementary ->
            Encode.string "supplementary"

        Slide ->
            Encode.string "slide"
