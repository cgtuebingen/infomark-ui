module Api.Endpoint exposing 
    ( Endpoint(..)
    , account
    , accountAvatar
    , accountEnrollment
    , confirmEmail
    , course
    , courseEnrollment
    , courseEnrollmentUserDetail
    , courseGroup
    , courseGroupBids
    , courseGroups
    , courses
    , groups
    , groupsEnrollment
    , updatePassword
    , requestPasswordReset
    , sessions
    , unwrap
    , user
    , users)

import Api.Data.Course exposing (Course)
import Http
import Url.Builder exposing (QueryParameter)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


basePath : String
basePath =
    "http://localhost:3000"



-- "http://laburnum.informatik.uni-tuebingen.de:3000"


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin basePath
        ("api" :: "v1" :: paths)
        queryParams
        |> Endpoint


sessions : Endpoint
sessions =
    url [ "auth", "sessions" ] []


requestPasswordReset : Endpoint
requestPasswordReset =
    url [ "auth", "request_password_reset" ] []


updatePassword : Endpoint
updatePassword =
    url [ "auth", "update_password" ] []


confirmEmail : Endpoint
confirmEmail =
    url [ "auth", "confirm_email" ] []


account : Endpoint
account =
    url [ "account" ] []


accountAvatar : Endpoint
accountAvatar =
    url [ "account", "avatar" ] []


accountEnrollment : Endpoint
accountEnrollment =
    url [ "account", "enrollments" ] []


users : Endpoint
users =
    url [ "users" ] []


user : Int -> Endpoint
user id =
    url [ "users", String.fromInt id ] []


courses : Endpoint
courses =
    url [ "courses" ] []


course : Int -> Endpoint
course id =
    url [ "courses", String.fromInt id ] []


courseEnrollment : Int -> List QueryParameter -> Endpoint
courseEnrollment id params =
    url [ "courses", String.fromInt id, "enrollments" ] params


courseGroups : Int -> Endpoint
courseGroups id =
    url [ "courses", String.fromInt id, "groups" ] []


courseGroup : Int -> Endpoint
courseGroup id =
    url [ "courses", String.fromInt id, "group" ] []


courseGroupBids : Int -> Endpoint
courseGroupBids id =
    url [ "courses", String.fromInt id, "bids" ] []



--offset, limit, roles


courseEnrollmentUserDetail : Int -> Int -> Endpoint
courseEnrollmentUserDetail courseId userId =
    url [ "courses", String.fromInt courseId, "enrollments", String.fromInt userId ] []


groups : Int -> Endpoint
groups id =
    url [ "groups", String.fromInt id ] []


groupsEnrollment : Int -> Endpoint
groupsEnrollment id =
    url [ "groups", String.fromInt id, "enrollments" ] []
