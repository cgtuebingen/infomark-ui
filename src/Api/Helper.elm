module Api.Helper exposing (..)

import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Http

-- TODO MOVE EVERYTHIN BACK TO REQUEST INSTEAD OF RISKYREQUEST

get : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
get url msg decoder =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
post url body msg decoder =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }

patch : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
patch url body msg decoder =
    Http.riskyRequest
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }

delete : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
delete url msg decoder =
    Http.riskyRequest
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }