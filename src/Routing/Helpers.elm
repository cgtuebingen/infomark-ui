module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>))


type Route
    = LoginRoute
    | RegistrationRoute
    | HomeRoute
    | NotFoundRoute


reverseRoute : Route -> String
reverseRoute route =
    case route of
        LoginRoute ->
            "#/login"

        RegistrationRoute ->
            "#/registration"

        _ ->
            "#/"


routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map HomeRoute Url.Parser.top
        , Url.Parser.map LoginRoute (Url.Parser.s "login")
        , Url.Parser.map RegistrationRoute (Url.Parser.s "registration")
        ]


parseUrl : Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            HomeRoute

        Just fragment ->
            { url | path = fragment, fragment = Nothing }
                |> Url.Parser.parse routeParser
                |> Maybe.withDefault NotFoundRoute