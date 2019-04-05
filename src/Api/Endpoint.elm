module Api.Endpoint exposing
    ( Endpoint(..)
    , account
    , accountAvatar
    , accountEnrollment
    , confirmEmail
    , course
    , courseBids
    , courseEmail
    , courseEnrollment
    , courseEnrollmentUserDetail
    , courseGrade
    , courseGrades
    , courseGroupBid
    , courseGroups
    , courseGroupsOwn
    , courseMaterials
    , courseMissingGrades
    , courseMissingTasks
    , coursePoints
    , courseSheets
    , courses
    , grade
    , group
    , groupEmail
    , groupsEnrollment
    , material
    , materialFile
    , me
    , requestPasswordReset
    , sessions
    , sheet
    , sheetFile
    , sheetPoints
    , sheetTasks
    , submissionFile
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


courseEmail : Int -> Endpoint
courseEmail id =
    url [ "courses", String.fromInt id, "emails" ] []


groupEmail : Int -> Int -> Endpoint
groupEmail courseId groupId =
    url
        [ "courses"
        , String.fromInt courseId
        , String.fromInt groupId
        , "emails"
        ]
        []


courseSheets : Int -> Endpoint
courseSheets id =
    url [ "courses", String.fromInt id, "sheets" ] []


courseMaterials : Int -> Endpoint
courseMaterials id =
    url [ "courses", String.fromInt id, "materials" ] []


courseEnrollment : Int -> List QueryParameter -> Endpoint
courseEnrollment id params =
    url [ "courses", String.fromInt id, "enrollments" ] params


courseGroups : Int -> Endpoint
courseGroups id =
    url [ "courses", String.fromInt id, "groups" ] []


courseGroupsOwn : Int -> Endpoint
courseGroupsOwn id =
    url [ "courses", String.fromInt id, "groups", "own" ] []


courseGroupBid : Int -> Int -> Endpoint
courseGroupBid courseId groupId =
    url [ "courses", String.fromInt courseId, "groups", String.fromInt groupId, "bids" ] []


courseBids : Int -> Endpoint
courseBids courseId =
    url [ "courses", String.fromInt courseId, "bids" ] []


courseEnrollmentUserDetail : Int -> Int -> Endpoint
courseEnrollmentUserDetail courseId userId =
    url [ "courses", String.fromInt courseId, "enrollments", String.fromInt userId ] []


coursePoints : Int -> Endpoint
coursePoints courseId =
    url [ "courses", String.fromInt courseId, "points" ] []


courseMissingGrades : Int -> Endpoint
courseMissingGrades courseId =
    url [ "courses", String.fromInt courseId, "grades", "missing" ] []


courseMissingTasks : Int -> Endpoint
courseMissingTasks courseId =
    url [ "courses", String.fromInt courseId, "tasks", "missing" ] []


courseGrades : Int -> List QueryParameter -> Endpoint
courseGrades courseId params =
    url [ "courses", String.fromInt courseId, "grades" ] params


courseGrade : Int -> Int -> Endpoint
courseGrade courseId gradeId =
    url [ "courses", String.fromInt courseId, "grades", String.fromInt gradeId ] []


group : Int -> Int -> Endpoint
group courseId id =
    url [ "courses", String.fromInt courseId, "groups", String.fromInt id ] []


groupsEnrollment : Int -> Int -> List QueryParameter -> Endpoint
groupsEnrollment courseId id params =
    url [ "courses", String.fromInt courseId, "groups", String.fromInt id, "enrollments" ] params


material : Int -> Int -> Endpoint
material courseId materialId =
    url [ "courses", String.fromInt courseId, "materials", String.fromInt materialId ] []


materialFile : Int -> Int -> Endpoint
materialFile courseId materialId =
    url [ "courses", String.fromInt courseId, "materials", String.fromInt materialId, "file" ] []


sheet : Int -> Int -> Endpoint
sheet courseId id =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt id ] []


sheetFile : Int -> Int -> Endpoint
sheetFile courseId id =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt id, "file" ] []


sheetTasks : Int -> Int -> Endpoint
sheetTasks courseId id =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt id, "tasks" ] []


sheetPoints : Int -> Int -> Endpoint
sheetPoints courseId sheetId =
    url [ "courses", String.fromInt courseId, "sheets", String.fromInt sheetId, "points" ] []


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


submissionFile : Int -> Int -> Endpoint
submissionFile courseId submissionId =
    url [ "courses", String.fromInt courseId, "submissions", String.fromInt submissionId, "file" ] []


grade : Int -> Int -> Endpoint
grade courseId gradeId =
    url [ "courses", String.fromInt courseId, "grades", String.fromInt gradeId ] []
