module Components.TaskViewer exposing
    ( Msg(..)
    , Model
    , init
    , update
    , view
    )


{-| The non admin counter part of TaskEditor. 
Can be used to upload submissions and view the
public test results.
-}

import Api.Data.Grade exposing (Grade)
import Api.Data.Task exposing (Task)
import Api.Request.Task as TaskRequests
import Components.CommonElements exposing (sliderInputElement, fileUploader)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles


type Field
    = Rating


type Msg
    = UploadSubmission
    | UploadSubmissionResponse (WebData ())
    | DoneUploading
    | GetGradeResponse (WebData Grade) -- Change return type
    | RateTask Int
    | RateResponse (WebData ())
    | ToggleCollapse
    | GotFiles File (List File)
    | Pick
    | DragEnter
    | DragLeave


type alias Model =
    { task : Task 
    , gradeResponse : WebData Grade
    , rating : Int
    , submission : Maybe File
    , collapse : Bool
    , hover : Bool
    }


init : Task -> (Model, Cmd Msg)
init task =
    ( 
        { task = task 
        , gradeResponse = Loading
        , rating = 0
        , submission = Nothing
        , collapse = True
        , hover = False
        }
    , TaskRequests.taskResultGet task.id GetGradeResponse
    )

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UploadSubmission ->
            (model, Cmd.none, NoUpdate)

        UploadSubmissionResponse response ->
            (model, Cmd.none, NoUpdate)
        
        DoneUploading ->
            (model, Cmd.none, NoUpdate)
    
        GetGradeResponse response ->
            (model, Cmd.none, NoUpdate)

        RateTask rating ->
            (model, Cmd.none, NoUpdate)

        RateResponse response ->
            (model, Cmd.none, NoUpdate)

        ToggleCollapse ->
            ( { model | collapse = not model.collapse }, Cmd.none, NoUpdate )

        GotFiles file files ->
            ( { model | hover = False, submission = Just file }, Cmd.none, NoUpdate )

        Pick ->
            ( model, Select.files [ "application/zip" ] GotFiles, NoUpdate )
    
        DragEnter ->
            ( { model | hover = True }, Cmd.none, NoUpdate )

        DragLeave ->
            ( { model | hover = False }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [ classes [ TC.w_100 ] ] <|
        [ div
            [ classes
                [ TC.w_100
                , TC.flex
                , TC.flex_row
                , TC.justify_between
                , TC.items_center
                , if model.collapse then
                    TC.mb3

                  else
                    TC.mb0
                ]
            ] 
            []
        ]
        

type alias Error =
    ( Field, String )