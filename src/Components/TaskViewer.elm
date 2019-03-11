module Components.TaskViewer exposing
    ( Msg(..)
    )


{-| The non admin counter part of TaskEditor. 
Can be used to upload submissions and view the
public test results.
-}

import Api.Data.Grade exposing (Grade)
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))

type Field
    = Rating


type Msg
    = UploadSubmission
    | UploadSubmissionResponse (WebData ())
    | GetGradeResponse (WebData Grade) -- Change return type
    | RateTask Int
    | RateResponse (WebData ())