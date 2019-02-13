{-
    This is the login site. Here, tutors and students should get
    an overview about what to do next.
        - Users should see:
            - All enrolled courses
        - Students should see (per course):
            - An overview of their points for each sheet
            - The total amount of points acquired
            - The needed amount of points to pass the course
            - What tasks are missing
        - Tutors should see (per course):
            - The point distribution per exercise sheet of the course or group?
            - The tasks which are not done grading
-}

module Pages.Dashboard exposing (..)

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