module Api.Data.PointOverview exposing (PointOverview, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode

type alias PointOverview =
    { acquired_points : Int
    , max_points : Int
    , sheet_id : Maybe Int
    , task_id : Maybe Int
    }

decoder : Decoder PointOverview
decoder =
    Decode.succeed PointOverview
        |> required "acquired_points" Decode.int
        |> required "max_points" Decode.int
        |> optional "sheet_id" (Decode.nullable Decode.int) Nothing
        |> optional "task_id" (Decode.nullable Decode.int) Nothing

encoder : PointOverview -> Encode.Value
encoder model =
    Encode.object
        [ ( "acquired_points", Encode.int model.acquired_points )
        , ( "max_points", Encode.int model.max_points )
        , case (model.sheet_id, model.task_id) of
            (Just id, _) ->
                ( "sheet_id", Encode.int id )
            
            (_, Just id) ->
                ( "task_id", Encode.int id )

            (_, _) ->
                ( "", Encode.null )
        ]