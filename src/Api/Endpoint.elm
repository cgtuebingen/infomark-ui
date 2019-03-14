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
    , courseSheets
    , courses
    , groups
    , groupsEnrollment
    , me
    , requestPasswordReset
    , sessions
    , sheet
    , sheetFile
    , sheetTasks
    , submissions
    , task
    , taskPrivateFiles
    , taskPublicFiles
    , taskRating
    , taskResult
    , taskSubmission
    , unwrap
    , updatePassword
    , user
    , users
    )

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


me : Endpoint
me =
    url [ "me" ] []


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


courseSheets : Int -> Endpoint
courseSheets id =
    url [ "courses", String.fromInt id, "sheets" ] []


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


courseEnrollmentUserDetail : Int -> Int -> Endpoint
courseEnrollmentUserDetail courseId userId =
    url [ "courses", String.fromInt courseId, "enrollments", String.fromInt userId ] []


groups : Int -> Int -> Endpoint
groups courseId id =
    url [ "courses", String.fromInt courseId, "groups", String.fromInt id ] []


groupsEnrollment : Int -> Int -> Endpoint
groupsEnrollment courseId id =
    url [ "courses", String.fromInt courseId, "groups", String.fromInt id, "enrollments" ] []


sheet : Int -> Int -> Endpoint
sheet courseId id =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt id ] []


sheetFile : Int -> Int -> Endpoint
sheetFile courseId id =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt id, "file" ] []


sheetTasks : Int -> Int -> Endpoint
sheetTasks courseId id =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt id, "tasks" ] []


task : Int -> Int -> Endpoint
task courseId id =
    url [ "courses", String.fromInt courseId, "tasks", String.fromInt id ] []


taskPublicFiles : Int -> Int -> Endpoint
taskPublicFiles courseId id =
    url [ "courses", String.fromInt courseId, "tasks", String.fromInt id, "public_file" ] []


taskPrivateFiles : Int -> Int -> Endpoint
taskPrivateFiles courseId id =
    url [ "courses", String.fromInt courseId, "tasks", String.fromInt id, "private_file" ] []


taskResult : Int -> Int -> Endpoint
taskResult courseId id =
    url [ "courses", String.fromInt courseId, "tasks", String.fromInt id, "result" ] []


taskRating : Int -> Int -> Endpoint
taskRating courseId taskId =
    url [ "courses", String.fromInt courseId, "tasks", String.fromInt taskId, "ratings" ] []


taskSubmission : Int -> Int -> Endpoint
taskSubmission courseId taskId =
    url [ "courses", String.fromInt courseId, "tasks", String.fromInt taskId, "submission" ] []


submissions : Int -> List QueryParameter -> Endpoint
submissions courseId params =
    url [ "courses", String.fromInt courseId, "submissions" ] params
