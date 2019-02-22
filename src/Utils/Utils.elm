module Utils.Utils exposing (perform, handleLogoutErrors)

import Task
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Browser.Navigation exposing (pushUrl)
import Routing.Helpers exposing (Route(..), reverseRoute)
import Http


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed

handleLogoutErrors : model -> SharedState -> (Http.Error -> (model, Cmd msg, SharedStateUpdate)) -> Http.Error -> (model, Cmd msg, SharedStateUpdate)
handleLogoutErrors model sharedState handler err =
    case err of
        Http.BadStatus 403 -> 
            case sharedState.userMail of
                Just _ -> (model, Cmd.none, RefreshLogin)
                Nothing -> (model, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate)

        _ -> handler err
