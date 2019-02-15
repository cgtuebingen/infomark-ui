module Api.Request.Courses exposing (courseDelete, courseGet, coursePatch, coursesGet, coursesPost)

import Api.Data.Course as Course exposing (Course)
import Api.Data.Error as Error exposing (Error)
import Api.Endpoint exposing (course, courses, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


coursesPost : Course -> (WebData Course -> msg) -> Cmd msg
coursesPost courseNew msg =
    post (unwrap courses)
        (Http.jsonBody (Course.encoder courseNew))
        msg
        Course.decoder


coursesGet : (WebData (List Course) -> msg) -> Cmd msg
coursesGet msg =
    get (unwrap courses)
        msg
    <|
        Decode.list Course.decoder


courseGet : Int -> (WebData Course -> msg) -> Cmd msg
courseGet id msg =
    get (unwrap <| course id)
        msg
        Course.decoder


coursePatch : Int -> Course -> (WebData String -> msg) -> Cmd msg
coursePatch id courseUp msg =
    patch (unwrap <| course id)
        (Http.jsonBody (Course.encoder courseUp))
        msg
        Decode.string


courseDelete : Int -> (WebData String -> msg) -> Cmd msg
courseDelete id msg =
    delete (unwrap <| course id)
        msg
        Decode.string
