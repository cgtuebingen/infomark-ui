module Decoders exposing (decodeTranslations)

import Json.Decode exposing (Decoder, dict, string)
import Types exposing (Translations)


decodeTranslations : Decoder Translations
decodeTranslations =
    dict string
