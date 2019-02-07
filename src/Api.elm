port module Api exposing (Cred)

import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode

type Cred 
    = Cred String String 


username : Cred -> String
username (Cred val _) =
    val


credHeader : Cred -> Http.Header
credHeader (Cred _ str) =
    Http.header "authorization" ("Token " ++ str)

{-|
    DO NOT EXPOSE THIS
    
    Use login instead. The only way to access Cred should be via flags or login endpoint
-}
credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Decode.string
        |> required "token" Decode.string


-- PERSISTENCE

decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)

port onStoreChange : (Value -> msg) -> Sub msg


storeCredWith : Cred -> Cmd msg
storeCredWith (Cred uname token) =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Encode.string uname )
                        , ( "token", Encode.string token )
                        ]
                  )
                ]
    in
    storeCache (Just json)

port storeCache : Maybe Value -> Cmd msg


-- HTTP

get : Endpoint -> Maybe Cred -> Decoder a -> Http.Request a
get url maybeCred decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }


put : Endpoint -> Cred -> Body -> Decoder a -> Http.Request a
put url cred body decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


post : Endpoint -> Maybe Cred -> Body -> Decoder a -> Http.Request a
post url maybeCred body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


delete : Endpoint -> Cred -> Body -> Decoder a -> Http.Request a
delete url cred body decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }

decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder

    
-- LOCALSTORAGE KEYS

cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"