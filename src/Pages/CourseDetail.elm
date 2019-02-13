{-
    This is the course detail page. It displays:
    - Basic information (Title, Description, Start and Ends dates etc.)
    - The course tutors as avatars with email, name etc
        - If you are an root user:
            - Option to search for all enrolled users and change the enrollment (tutor/student)
    - Group information:
        - If you are a student and not distributed to a group:
            - The exercise groups with dates and tutors
            - Here you can set your preferences
        - If you are a student and distributed to a group:
            - Your group
            - Maybe an inline forum view or a link to the group forum
        - If you are a tutor/supertutor:
            - Your group with date and times
            - A link to send emails (inline?)
            - Maybe an inline forum view or a link to the group forum
        - If you are an root:
            - Options to create/edit/delete groups
            - Options to view all users in a group and an option to change a user from one group to another
    - Exercise sheets (download and link to the sheet for uploading/grading)
        - If you are an root:
            - Options to create/edit/delete sheets -> Extra view
    - Other course materials (Slides and Supplementary) (downloadable)
        - If you are an root:
            - Options to create/edit/delete materials (Inline?)
    - Statistics for Tutors and root
-}

module Pages.CourseDetail exposing (..)

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