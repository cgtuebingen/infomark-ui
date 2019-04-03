module Utils.Utils exposing
    ( flip
    , handleLogoutErrors
    , perform
    , split
    , tupleExtend
    , tupleMapThree
    , unzipTripple
    , delay
    )

import Browser.Navigation exposing (pushUrl)
import Http
import List
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Task
import Time
import Process


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed

delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)

handleLogoutErrors : model -> SharedState -> (Http.Error -> ( model, Cmd msg, SharedStateUpdate )) -> Http.Error -> ( model, Cmd msg, SharedStateUpdate )
handleLogoutErrors model sharedState handler err =
    {- let
           _ =
               Debug.log "Received error code" err
       in
    -}
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


tupleExtend : ( a, b ) -> c -> ( a, b, c )
tupleExtend ( a, b ) c =
    ( a, b, c )


unzipTripple : List ( a, b, c ) -> ( List a, List b, List c )
unzipTripple tripples =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) tripples
