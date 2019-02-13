{-
    Additional statistics about:
        - Readiness for work of tutors
            - How many missing graded submissions
        - Readiness for work of students
            - How many students stopped at submitting for sheet x
-}

module Pages.AdminView exposing (..)

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