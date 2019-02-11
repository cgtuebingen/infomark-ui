module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Browser.Navigation
import Time exposing (Posix, Zone)
import Types exposing (Translations)
import Api.Data.Role as Role exposing (Role)


type alias SharedState = 
    { navKey : Browser.Navigation.Key
    , currentTime : Maybe Posix
    , timezone : Maybe Zone
    , translations : Translations
    , role : Maybe Role
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateTimezone Zone
    | UpdateTranslations Translations
    | UpdateRole (Maybe Role)


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = Just time }

        UpdateTimezone zone ->
            { sharedState | timezone = Just zone }

        UpdateTranslations translations ->
            { sharedState | translations = translations }

        UpdateRole role ->
            { sharedState | role = role }

        NoUpdate ->
            sharedState