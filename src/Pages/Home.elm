{-
    This is the home site with links to all courses.
    If the user is not logged in redirect to the login page

    -- TODO: DEPRECATED: Delete this
-}
module Pages.Home exposing (..)

import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Http
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import RemoteData exposing (RemoteData(..), WebData)
import Api.Request.Account exposing (accountGet)
import Api.Data.User exposing (User) 


type alias Model =
    { userProgress : (WebData User)
    }


type Msg
    = AccountResponse (WebData User)
    | NavigateTo Route
     

init : ( Model, Cmd Msg)
init =
    ( { userProgress = Loading
      }
    , accountGet AccountResponse
    )

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        AccountResponse response ->
            ({model | userProgress = response }, Cmd.none, NoUpdate)

        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

view : SharedState -> Model -> Html Msg
view sharedState model = div [] [ viewAccountOrError model.userProgress ]

viewAccountOrError : WebData User -> Html Msg
viewAccountOrError data =
    let 
        _ = Debug.log "Data:" data
    in
    case data of
        RemoteData.Success user ->
            text user.email

        RemoteData.Failure (Http.BadStatus 400) ->
            text "Wrong Format"

        RemoteData.Failure (Http.BadStatus 401) ->
            text "Not Logged In"

        RemoteData.Failure (Http.BadStatus 403) -> -- In this case navigate to login. And think of a way to set a error message. Maybe with sharedState
            text "Not permitted"

        _ ->
            text "Scary stuff"
