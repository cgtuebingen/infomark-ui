module Api.Request.Terms exposing (Terms, termsOfUseGet)

import Api.Endpoint exposing (terms, unwrap)
import Api.Helper exposing (get)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData(..), WebData)


type alias Terms =
    { terms : String
    }


decoder : Decoder Terms
decoder =
    Decode.succeed Terms
        |> required "text" Decode.string


termsOfUseGet : (WebData Terms -> msg) -> Cmd msg
termsOfUseGet msg =
    get (unwrap <| terms) msg decoder
