module Api.HelperMock exposing (delete, get, patch, post)

import Http
import Http.Mock
import Json.Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)


get : String -> (WebData a -> msg) -> Decoder a -> Http.Response String -> Cmd msg
get url msg decoder mock =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.Mock.expectJson mock (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Http.Response String -> Cmd msg
post url body msg decoder mock =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.Mock.expectJson mock (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


patch : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Http.Response String -> Cmd msg
patch url body msg decoder mock =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.Mock.expectJson mock (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> (WebData a -> msg) -> Decoder a -> Http.Response String -> Cmd msg
delete url msg decoder mock =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.Mock.expectJson mock (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }
