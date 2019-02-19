module Api.Request.UserEnrollments exposing (..)

import Api.Data.UserEnrollment as UserEnrollment exposing (UserEnrollment)
import Api.Data.User as User exposing (User)
import Api.Data.CourseRole as CourseRole exposing (CourseRole(..))
import Api.Data.Error as Error exposing (Error)
import Api.Endpoint exposing (courseEnrollment, courseEnrollmentUserDetail, unwrap)
import Api.Helper exposing (..)
import Decoders
import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder exposing (QueryParameter)

courseEnrollmentGet : Int -> List QueryParameter -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
courseEnrollmentGet courseId params msg =
    get (unwrap <| courseEnrollment courseId params)
        msg
    <|
        Decode.list UserEnrollment.decoder


courseEnrollmentGetAll : Int -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
courseEnrollmentGetAll courseId msg =
    courseEnrollmentGet courseId [] msg


courseEnrollmentGetTeam : Int -> (WebData (List UserEnrollment) -> msg) -> Cmd msg
courseEnrollmentGetTeam courseId msg =
    let
        roles = String.join "," <|
                List.map String.fromInt <|
                    List.map CourseRole.roleToInt [Admin, Tutor]
        params = [Url.Builder.string "roles" roles ]
    in
    courseEnrollmentGet courseId params msg