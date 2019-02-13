{-
    This is the sheet detail page. It displays:
        - Basic info such as name, dates, a warning if the submission is already closed
        - The link to download the PDF instructions
        - The tasks as a list - Maybe as cards:
            - If you are a student:
                - Option to rate the task and upload your submission
                - Small view about the test results
                - After ratings are finished show the feedback and points
            - If you are an root:
                - Option to create/edit/delete tasks
            - If you are a supertutor:
                - Link to the tasks create/editing view
            - If you are a tutor/supertutor:
                - Show the following per default for your own group/s. 
                  Make it overrideable to other groups in case of ill tutors:
                    - Link to the grading view for each task after submission deadline
                    - Link to download all submissions for the task and sheet
        - Ratings/Reviews
            - If you are a student:
                - The gradings as a statistic of your sheet
            - If you are a root/tutor/supertutor
                - The students ratings as a statistic
-}

module Pages.SheetDetail exposing (..)

import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import RemoteData exposing (RemoteData(..), WebData)
import Utils.Styles as Styles
import Time
import I18n
import Api.Data.Course exposing (Course)

type Msg
    = NavigateTo Route