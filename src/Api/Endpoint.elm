module Api.Endpoint exposing (Endpoint(..), account, basePath, sessions, unwrap, url)

import Http
import Url.Builder exposing (QueryParameter)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


basePath : String
basePath =
    "http://localhost:3000"



-- "http://laburnum.informatik.uni-tuebingen.de:3000"


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin basePath
        ("api" :: "v1" :: paths)
        queryParams
        |> Endpoint



-- ENDPOINTS


sessions : Endpoint
sessions =
    url [ "auth", "sessions" ] []


account : Endpoint
account =
    url [ "account" ] []
