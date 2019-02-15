module Utils.Utils exposing (..)

import Task

perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed