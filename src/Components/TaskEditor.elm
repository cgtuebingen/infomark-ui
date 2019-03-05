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
import Html.Events exposing (onClick, onInput, onSubmit, preventDefaultOn)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Utils.Styles as Styles
import File exposing (File)
import File.Select as Select
import Json.Decode as Decode exposing (Decoder)
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
            ( model |>
                updateHover fileType False |>
                    updateFile fileType (Just file)
            , Cmd.none, NoUpdate)

        Pick fileType ->
            (model, Select.files [ "application/zip" ] (GotFiles fileType), NoUpdate)
        
        DragEnter fileType ->
            (updateHover fileType True model, Cmd.none, NoUpdate)
        
        DragLeave fileType ->
            (updateHover fileType False model, Cmd.none, NoUpdate)
    
        FileUploadResponse fileType response ->
            (model, Cmd.none, NoUpdate)

        TaskGetRequest (Success task) ->
            (fillModelFromTask model task, Cmd.none, NoUpdate)

        TaskGetRequest response ->
            (model, Cmd.none, NoUpdate)


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [ classes [TC.w_100] ]
        [ div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ fileUploader model Public
            , fileUploader model Private
            ]
        , div [ classes [ TC.mt3, TC.mt4_ns, TC.cf, TC.ph2_ns ] ]
            [ text <| (String.fromInt model.id) ++ "/" ++(String.fromInt model.max_points)]
        ]
        

chooseHover : FileType -> (Model -> Bool)
chooseHover fileType =
    case fileType of
        Public -> .public_test_hover
        Private -> .private_test_hover 


chooseFile : FileType -> (Model -> Maybe File)
chooseFile fileType =
    case fileType of
        Public -> .public_test_file
        Private -> .private_test_file


updateHover : FileType -> Bool -> Model -> Model
updateHover fileType val model =
    case fileType of
        Public -> {model | public_test_hover = val }
        Private -> {model | private_test_hover = val }


updateFile : FileType -> Maybe File -> Model -> Model
updateFile fileType val model =
    case fileType of
        Public -> {model | public_test_file = val }
        Private -> {model | private_test_file = val }


fileUploader : Model -> FileType -> Html Msg
fileUploader model fileType =
    div
        [ classes
            [ TC.pa4
            , TC.ba
            , TC.b__dashed
            , if chooseHover fileType model then
                TC.b__dark_red
              else
                TC.b__black_40
            , TC.bw2
            , TC.br3
            , TC.w_100
            , TC.w_50_ns

            , TC.flex
            , TC.flex_column
            , TC.justify_center
            , TC.items_center
            , TC.fl
            ]
        , hijackOn "dragenter" (Decode.succeed <| DragEnter fileType)
        , hijackOn "dragover" (Decode.succeed <| DragEnter fileType)
        , hijackOn "dragleave" (Decode.succeed <| DragLeave fileType)
        , hijackOn "drop" (dropDecoder fileType)
        ]
        [ span
            [ Styles.labelStyle
            ]
            [ text <| Maybe.withDefault "" <| Maybe.map File.name <| chooseFile fileType model ]
        , button
            [ Styles.buttonGreyStyle
            , classes
                [ TC.w_100
                , TC.mt4
                ]
            , onClick <| Pick fileType
            ]
            [ text "Pick file" ]
        ]

dropDecoder : FileType -> Decoder Msg
dropDecoder fileType =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore (GotFiles fileType) File.decoder)


hijackOn : String -> Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
