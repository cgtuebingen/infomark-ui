module Types exposing (Language(..), Translations, languageToBackendString)

import Dict exposing (Dict)
import Iso8601
import Json.Decode exposing (Decoder, at, dict, field, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Time exposing (Posix)


type Language
    = English
    | German


type alias Translations =
    Dict String String


decodeTranslations : Decoder Translations
decodeTranslations =
    dict string


languageToBackendString : Language -> String
languageToBackendString lang =
    case lang of
        English -> "en"
        German -> "de"
