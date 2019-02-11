module Api.Data.Material exposing (Material, decoder, encoder)

import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time exposing (Posix)


type MaterialType
    = Supplementary
    | Slide


type alias Material =
    { id : Int
    , file_url : Maybe String
    , name : String
    , material_type : MaterialType -- TODO enum? supplementary or slide
    , published_at : Posix
    , lecture_at : Posix
    }


decoder : Decoder Material
decoder =
    Decode.succeed Material
        |> required "id" Decode.int
        |> optional "file_url" (Decode.nullable Decode.string) Nothing
        |> required "name" Decode.string
        |> required "material_type" typeDecoder
        |> required "published_at" Iso8601.decoder
        |> required "lecture_at" Iso8601.decoder


encoder : Material -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "file_url", Maybe.withDefault Encode.null <| Maybe.map Encode.string model.file_url )
        , ( "name", Encode.string model.name )
        , ( "material_type", typeEncoder model.material_type )
        , ( "published_at", Iso8601.encode model.published_at )
        , ( "lecture_at", Iso8601.encode model.lecture_at )
        ]


typeDecoder : Decoder MaterialType
typeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "slide" ->
                        Decode.succeed Slide

                    _ ->
                        Decode.succeed Supplementary
            )


typeEncoder : MaterialType -> Encode.Value
typeEncoder materialType =
    case materialType of
        Supplementary ->
            Encode.string "supplementary"

        Slide ->
            Encode.string "slide"
