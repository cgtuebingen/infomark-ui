module Decoders exposing (..)

import Json.Decode exposing (Decoder, at, dict, field, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required, requiredAt, optional, optionalAt)
import Time exposing (Posix)
import Types exposing (..)


decodeTranslations : Decoder Translations
decodeTranslations =
    dict string