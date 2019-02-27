module Utils.Utils exposing (handleLogoutErrors, perform)

import Browser.Navigation exposing (pushUrl)
import Http
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Task


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed


handleLogoutErrors : model -> SharedState -> (Http.Error -> ( model, Cmd msg, SharedStateUpdate )) -> Http.Error -> ( model, Cmd msg, SharedStateUpdate )
handleLogoutErrors model sharedState handler err =
    let
        _ =
            Debug.log "Received error code" err
    in
    case err of
        Http.BadStatus 401 ->
            case sharedState.userMail of
                Just _ ->
                    ( model, Cmd.none, RefreshLogin )

                Nothing ->
                    ( model, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        _ ->
            handler err
