module Utils.Utils exposing (flip, handleLogoutErrors, perform, split, tupleMapThree)

import Browser.Navigation exposing (pushUrl)
import Http
import List
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


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


tupleMapThree : (a -> x) -> (b -> y) -> (c -> z) -> ( a, b, c ) -> ( x, y, z )
tupleMapThree funcA funcB funcC ( x, y, z ) =
    ( funcA x, funcB y, funcC z )
