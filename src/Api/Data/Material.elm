module Api.Data.Material exposing (Material, MaterialType(..), decoder, encoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Time exposing (Posix)


type MaterialType
    = Supplementary
    | Slide


type alias Material =
    { id : Int
    , file_url : Maybe String
    , name : String
    , material_type : MaterialType
    , published_at : Posix
    , lecture_at : Posix
    }


decoder : Decoder Material
decoder =
    Decode.succeed Material
        |> required "id" Decode.int
        |> optional "file_url" (Decode.nullable Decode.string) Nothing
        |> required "name" Decode.string
        |> required "kind" typeDecoder
        |> required "publish_at" Iso8601.decoder
        |> required "lecture_at" Iso8601.decoder


encoder : Material -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "kind", typeEncoder model.material_type )
        , ( "publish_at", Iso8601.encode model.published_at )
        , ( "lecture_at", Iso8601.encode model.lecture_at )
        ]


typeDecoder : Decoder MaterialType
typeDecoder =
    Decode.int
        |> Decode.andThen
            (\v ->
                case v of
                    0 ->
                        Decode.succeed Slide

                    _ ->
                        Decode.succeed Supplementary
            )


typeEncoder : MaterialType -> Encode.Value
typeEncoder materialType =
    case materialType of
        Supplementary ->
            Encode.int 1

        Slide ->
            Encode.int 0
