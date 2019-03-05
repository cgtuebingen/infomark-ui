module Components.TaskEditor exposing
    ( Msg(..)
    , Model
    , initFromId
    , initFromTask
    , initCreate
    , update
    , view
    )

{-| Inline Task Editor and Creator

-}

import Api.Data.Task exposing (Task)
import Api.Request.Task as TaskRequests
import Html exposing (..)
import File exposing (File)
import File.Select as Select
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))


type FileType
    = Public
    | Private

type Field
    = MaxPoints
    | PublicDockerImage
    | PrivateDockerImage

type Msg
    = SendTask
    | SendPublicFiles
    | SendPrivateFile
    | SetField Field String
    | GotFiles FileType File (List File)
    | Pick FileType
    | DragEnter FileType
    | DragLeave FileType
    | FileUploadResponse FileType (WebData ())
    | TaskGetRequest (WebData Task)


type alias Model =
    { id : Int
    , sheet_id : Int 
    , max_points : Int
    , public_tests_url : Maybe String
    , private_tests_url : Maybe String
    , public_docker_image : Maybe String
    , private_docker_image : Maybe String
    , public_test_file : Maybe File
    , private_test_file : Maybe File
    , public_test_hover : Bool
    , private_test_hover : Bool
    }


initModel : (Model, Cmd Msg)
initModel =
    (
        { id = 0
        , sheet_id = 0
        , max_points = 0
        , public_tests_url = Nothing
        , private_tests_url = Nothing
        , public_docker_image = Nothing
        , private_docker_image = Nothing
        , public_test_file = Nothing
        , private_test_file = Nothing
        , public_test_hover = False
        , private_test_hover = False
        }
    , Cmd.none
    )


initCreate : Int -> (Model, Cmd Msg)
initCreate sheetId =
    let
        (model, cmds) = initModel
    in
    ( {model | sheet_id = sheetId }, cmds)
    

initFromId : Int -> (Model, Cmd Msg)
initFromId id =
    let
        (model, cmds) = initModel
    in
    ( { model | id = id }
    , Cmd.batch 
        [ cmds
        , TaskRequests.taskGet id TaskGetRequest
        ]
    )


initFromTask : Task -> (Model, Cmd Msg)
initFromTask task =
    let
        (model, cmds) = initModel
    in
    ( fillModelFromTask model task
    , cmds
    )


fillModelFromTask : Model -> Task -> Model
fillModelFromTask model task =
    { model 
        | id = task.id
        , max_points = task.max_points
        , public_tests_url = task.public_tests_url
        , private_tests_url = task.private_tests_url
        , public_docker_image = task.public_docker_image
        , private_docker_image = task.private_docker_image
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SendTask ->
            (model, Cmd.none, NoUpdate)
        
        SendPublicFiles ->
            (model, Cmd.none, NoUpdate)

        SendPrivateFile ->
            (model, Cmd.none, NoUpdate)

        SetField field value ->
            (model, Cmd.none, NoUpdate)

        GotFiles fileType file files ->
            (model, Cmd.none, NoUpdate)

        Pick fileType ->
            (model, Cmd.none, NoUpdate)
        
        DragEnter fileType ->
            (model, Cmd.none, NoUpdate)
        
        DragLeave fileType ->
            (model, Cmd.none, NoUpdate)
    
        FileUploadResponse fileType response ->
            (model, Cmd.none, NoUpdate)

        TaskGetRequest (Success task) ->
            (fillModelFromTask model task, Cmd.none, NoUpdate)

        TaskGetRequest response ->
            (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [] [ text <| (String.fromInt model.id) ++ "/" ++(String.fromInt model.max_points)]