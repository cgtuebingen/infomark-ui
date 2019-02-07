module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Browser.Navigation
import Time exposing (Posix)
import Types exposing (Translations)


type alias SharedState = 
    { navKey : Browser.Navigation.Key
    , currentTime : Posix
    , translations : Translations
    , token : Maybe String
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateTranslations Translations
    | UpdateToken (Maybe String)


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    let 
        _ = Debug.log "SharedStateUpdate" sharedStateUpdate
        _ = Debug.log "Old Shared State" sharedState
    in
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = time }

        UpdateTranslations translations ->
            { sharedState | translations = translations }

        UpdateToken token ->
            { sharedState | token = token }

        NoUpdate ->
            sharedState