module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Api.Data.Role as Role exposing (Role)
import Browser.Navigation
import Time exposing (Posix, Zone)
import Types exposing (Language(..), Translations)


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , currentTime : Maybe Posix
    , timezone : Maybe Zone
    , translations : Translations
    , selectedLanguage : Language
    , userMail : Maybe String
    , role : Maybe Role
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateTimezone Zone
    | UpdateLanguage Language Translations
    | UpdateRole (Maybe Role)
    | UpdateMail String
    | UpdateRoleAndMail Role String
    | RefreshLogin


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = Just time }

        UpdateTimezone zone ->
            { sharedState | timezone = Just zone }

        UpdateLanguage language translations ->
            { sharedState | translations = translations, selectedLanguage = language }

        UpdateRole role ->
            { sharedState | role = role }

        UpdateMail mail ->
            { sharedState | userMail = Just mail }

        UpdateRoleAndMail role mail ->
            { sharedState | userMail = Just mail, role = Just role }

        NoUpdate ->
            sharedState

        RefreshLogin ->
            sharedState
