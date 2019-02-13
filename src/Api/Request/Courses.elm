module Api.Request.Courses exposing (..)

import Api.Data.Courses as Course exposing (Course)
import Api.Data.Error as Error exposing (Error)
import Api.Endpoint exposing (courses, course, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)

coursesPost : Course -> (WebData Course -> msg) -> Cmd msg
coursesPost course msg =
    post (unwrap courses)
        (Http.jsonBody (Course.encoder course))
        msg
        Course.decoder


coursesGet : (WebData (List Course) -> msg) -> Cmd msg
coursesGet msg =
    get (unwrap courses)
        msg
        <| Decode.list Course.decoder


courseGet : Int -> (WebData Course -> msg) -> Cmd msg
courseGet id msg =
    get (unwrap <| course id)
        msg
        Course.decoder


coursePatch : Int -> Course -> (WebData String -> msg) -> Cmd msg
coursePatch id course msg =
    patch (unwrap <| course id)
        (Http.jsonBody (Course.encoder course))
        msg
        Decode.string

courseDelete : Int -> (WebData String -> msg) -> Cmd msg
courseDelete id msg =
    delete (unwrap <| course id)
        msg
        Decode.string
