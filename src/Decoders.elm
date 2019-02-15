module Decoders exposing (decodeTranslations)

import Json.Decode exposing (Decoder, at, dict, field, float, int, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Time exposing (Posix)
import Types exposing (..)


decodeTranslations : Decoder Translations
decodeTranslations =
    dict string
