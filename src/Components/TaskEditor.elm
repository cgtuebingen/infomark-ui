module Components.TaskEditor exposing
    ( Model
    , Msg(..)
    , initCreate
    , initFromId
    , initFromTask
    , update
    , view
    )

{-| Inline Task Editor and Creator
-}

import Api.Data.Task exposing (Task)
import Api.Request.Sheet as SheetRequests
import Api.Request.Task as TaskRequests
import Components.CommonElements exposing (inputLabel, inputElement, fileUploader, rContainer, rRow, rRowButton, rCollapsable, rRowExtraSpacing, r1Column, r2Column)
import File exposing (File)
import File.Select as Select
import Http
import Html exposing (..)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles
import Dict exposing (Dict)


type FileType
    = Public
    | Private

{-| Dicts with non comparable types are not allowed
So for this single instance we convert the types to something
comparable: https://github.com/elm/compiler/issues/1008#event-797842658
-}
fileTypeToInt : FileType -> Int
fileTypeToInt fileType =
    case fileType of
        Public -> 1
        Private -> 2

intToFileType : Int -> FileType
intToFileType fileTypeAsInt =
    if fileTypeAsInt == 1 then Public else Private

type Field
    = MaxPoints
    | PublicDockerImage
    | PrivateDockerImage


type Msg
    = SendTask
    | SetField Field String
    | GotFiles FileType File (List File)
    | Pick FileType
    | DragEnter FileType
    | DragLeave FileType
    | TaskGetRequest (WebData Task)
    | TaskCreateRequest (WebData Task)
    | TaskUpdateRequest (WebData ())
    | FileUploadResponse FileType (WebData ())
    | UploadProgress Http.Progress
    | DoneUploading
    | ToggleCollapse


type alias Model =
    { id : Int
    , courseId : Int
    , sheet_id : Int
    , max_points : String
    , public_tests_url : String
    , private_tests_url : String
    , public_docker_image : String
    , private_docker_image : String
    , public_test_file : Maybe File
    , private_test_file : Maybe File
    , public_test_hover : Bool
    , private_test_hover : Bool
    , collapse : Bool
    , createTask : Bool
    , toUpload : List FileType
    , uploading : Dict Int (WebData ())
    , averaged_progress : Int
    , errors : List Error
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { id = 0
      , courseId = 0
      , sheet_id = 0
      , max_points = "0"
      , public_tests_url = ""
      , private_tests_url = ""
      , public_docker_image = ""
      , private_docker_image = ""
      , public_test_file = Nothing
      , private_test_file = Nothing
      , public_test_hover = False
      , private_test_hover = False
      , collapse = True
      , createTask = False
      , toUpload = []
      , uploading = Dict.empty
      , averaged_progress = 0
      , errors = []
      }
    , Cmd.none
    )


initCreate : Int -> Int -> ( Model, Cmd Msg )
initCreate courseId sheetId =
    let
        ( model, cmds ) =
            initModel
    in
    ( { model | sheet_id = sheetId, courseId = courseId, createTask = True }, cmds )


initFromId : Int -> Int -> ( Model, Cmd Msg )
initFromId courseId id =
    let
        ( model, cmds ) =
            initModel
    in
    ( { model | id = id, courseId = courseId }
    , Cmd.batch
        [ cmds
        , TaskRequests.taskGet courseId id TaskGetRequest
        ]
    )


initFromTask : Int -> Task -> ( Model, Cmd Msg )
initFromTask courseId task =
    let
        ( inital, cmds ) =
            initModel
        
        model = { inital | courseId = courseId}
    in
    ( fillModelFromTask model task
    , cmds
    )


fillModelFromTask : Model -> Task -> Model
fillModelFromTask model task =
    { model
        | id = task.id
        , max_points = String.fromInt task.max_points
        , public_tests_url = Maybe.withDefault "" task.public_tests_url
        , private_tests_url = Maybe.withDefault "" task.private_tests_url
        , public_docker_image = Maybe.withDefault "" task.public_docker_image
        , private_docker_image = Maybe.withDefault "" task.private_docker_image
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SendTask ->
            ( model, createOrUpdate model, NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        GotFiles fileType file files ->
            ( model
                |> updateHover fileType False
                |> updateFile fileType (Just file)
            , Cmd.none
            , NoUpdate
            )

        Pick fileType ->
            ( model, Select.files [ "application/zip" ] (GotFiles fileType), NoUpdate )

        DragEnter fileType ->
            ( updateHover fileType True model, Cmd.none, NoUpdate )

        DragLeave fileType ->
            ( updateHover fileType False model, Cmd.none, NoUpdate )

        FileUploadResponse fileType response ->
            ( { model 
                | uploading = Dict.update 
                    (fileTypeToInt fileType) 
                    (Maybe.map (\_ -> response)) 
                    model.uploading
            }, Cmd.none, NoUpdate )

        UploadProgress progress ->
            let
                percentage = case progress of
                    Http.Sending p ->
                        Http.fractionSent p
                    _ -> 0.0
                
                fileTypes = model.uploading |>
                    Dict.toList |>
                        List.filter (\(_, state) -> state == Loading) |>
                        List.map (\(ftype, _) -> intToFileType ftype)

                prog = if List.length fileTypes > 0 then
                        -- If this particular task editor is uploading we need to calculate
                        -- the average upload progress
                        round <|
                            (
                                ( (toFloat model.averaged_progress) + 
                                    100 * percentage
                                ) / 2
                            ) / (toFloat <| List.length fileTypes)
                    else
                        0
            in
            ( { model | averaged_progress = prog }, Cmd.none, NoUpdate)

        TaskGetRequest (Success task) ->
            ( fillModelFromTask model task, Cmd.none, NoUpdate )

        TaskGetRequest response ->
            ( model, Cmd.none, NoUpdate )

        TaskCreateRequest (Success task) ->
            uploadFiles model

        TaskCreateRequest response ->
            ( model, Cmd.none, NoUpdate )

        TaskUpdateRequest (Success _) ->
            uploadFiles model
              
        TaskUpdateRequest response ->
            ( model, Cmd.none, NoUpdate )

        DoneUploading ->
            ( model, Cmd.none, NoUpdate )

        ToggleCollapse ->
            ( { model | collapse = not model.collapse }, Cmd.none, NoUpdate )


uploadFiles : Model -> (Model, Cmd Msg, SharedStateUpdate)
uploadFiles model =
    let
        (fileTypes, cmds) =
            case ( model.public_test_file, model.private_test_file ) of
                ( Just public, Just private ) ->
                    ( [ Public, Private ]
                    , Cmd.batch
                        [ TaskRequests.taskPublicFilesPost model.courseId model.id public (FileUploadResponse Public)
                        , TaskRequests.taskPrivateFilesPost model.courseId model.id private (FileUploadResponse Private)
                        ]
                    )
                ( Just public, _ ) ->
                    ( [Public], TaskRequests.taskPublicFilesPost model.courseId model.id public (FileUploadResponse Public))

                ( _, Just private ) ->
                    ( [Private], TaskRequests.taskPrivateFilesPost model.courseId model.id private (FileUploadResponse Private))

                (_, _) ->
                    ( [], Cmd.none)    
    in
    ( { model 
        | toUpload = fileTypes
        , uploading = Dict.fromList <| List.map (\ftype -> (fileTypeToInt ftype, Loading)) fileTypes
    }
    , cmds
    , NoUpdate
    )


view : SharedState -> Model -> Html Msg
view sharedState model =
    rContainer <|
        rCollapsable ("Task " ++ String.fromInt model.id) model.collapse ToggleCollapse ("Show", "Hide")
            [ rRow <|
                r2Column
                    [ inputLabel "Public Tests"
                    , fileUploader (chooseHover Public model) (chooseFile Public model) (DragEnter Public) (DragLeave Public) (Pick Public) (GotFiles Public)
                    ]
                    [ inputLabel "Private Tests"
                    , fileUploader (chooseHover Private model) (chooseFile Private model) (DragEnter Private) (DragLeave Private) (Pick Private) (GotFiles Private)
                    ]
            , rRowExtraSpacing <|
                r2Column
                    (inputElement
                        { label = "Public Tests Docker Image"
                        , placeholder = "Image Name"
                        , fieldType = "text"
                        , value = model.public_docker_image
                        }
                        PublicDockerImage
                        model.errors
                        SetField)
                    (inputElement
                        { label = "Private Tests Docker Image"
                        , placeholder = "Image Name"
                        , fieldType = "text"
                        , value = model.private_docker_image
                        }
                        PrivateDockerImage
                        model.errors
                        SetField)
            , rRow <|
                r1Column <|
                    inputElement
                        { label = "Max Points"
                        , placeholder = "Points"
                        , fieldType = "number"
                        , value = model.max_points
                        }
                        MaxPoints
                        model.errors
                        SetField
            , rRowButton 
                (if model.createTask then "Erstellen" else "Bearbeiten")
                SendTask
                (anythingUploading model || Dict.isEmpty model.uploading)
            ]


anythingUploading : Model -> Bool
anythingUploading model =
    model.uploading |>
        Dict.values |>
            List.any (\state -> state /= Loading)
    

setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        MaxPoints ->
            { model | max_points = value }

        PublicDockerImage ->
            { model | public_docker_image = value }

        PrivateDockerImage ->
            { model | private_docker_image = value }


createOrUpdate : Model -> Cmd Msg
createOrUpdate model =
    if model.createTask then
        SheetRequests.sheetTasksPost model.courseId model.sheet_id (fillTaskFromModel model) TaskCreateRequest

    else
        TaskRequests.taskPut model.courseId model.id (fillTaskFromModel model) TaskUpdateRequest


fillTaskFromModel : Model -> Task
fillTaskFromModel model =
    { id = 0
    , max_points = Maybe.withDefault 0 <| String.toInt model.max_points
    , public_tests_url = Nothing
    , private_tests_url = Nothing
    , public_docker_image = Just model.public_docker_image
    , private_docker_image = Just model.private_docker_image
    }


chooseHover : FileType -> (Model -> Bool)
chooseHover fileType =
    case fileType of
        Public ->
            .public_test_hover

        Private ->
            .private_test_hover


chooseFile : FileType -> (Model -> Maybe File)
chooseFile fileType =
    case fileType of
        Public ->
            .public_test_file

        Private ->
            .private_test_file


updateHover : FileType -> Bool -> Model -> Model
updateHover fileType val model =
    case fileType of
        Public ->
            { model | public_test_hover = val }

        Private ->
            { model | private_test_hover = val }


updateFile : FileType -> Maybe File -> Model -> Model
updateFile fileType val model =
    case fileType of
        Public ->
            { model | public_test_file = val }

        Private ->
            { model | private_test_file = val }


type alias Error =
    ( Field, String )
