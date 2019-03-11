module I18n exposing (get)

import Dict
import Types exposing (Translations)


get : Translations -> String -> String
get dict key =
    dict
        |> Dict.get key
        |> Maybe.withDefault key
