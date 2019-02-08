module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Browser.Navigation
import Time exposing (Posix)
import Types exposing (Translations)
import Api.Data.Role as Role exposing (Role)


type alias SharedState = 
    { navKey : Browser.Navigation.Key
    , currentTime : Posix
    , translations : Translations
    , role : Maybe Role
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateTranslations Translations
    | UpdateRole (Maybe Role)


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = time }

        UpdateTranslations translations ->
            { sharedState | translations = translations }

        UpdateRole role ->
            { sharedState | role = role }

        NoUpdate ->
            sharedState