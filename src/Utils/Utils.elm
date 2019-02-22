module Utils.Utils exposing (perform, handleLogoutErrors)

import Task
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Http


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed

handleLogoutErrors : model -> SharedState -> (Http.Error -> (model, Cmd msg, SharedStateUpdate)) -> Http.Error -> (model, Cmd msg, SharedStateUpdate)
handleLogoutErrors model sharedState handler err =
    case err of
        Http.BadStatus 403 -> (model, Cmd.none, RefreshLogin)

        _ -> handler err
