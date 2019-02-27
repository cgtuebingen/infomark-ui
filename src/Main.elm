module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Decoders
import Html exposing (..)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Router as Router
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Spinner
import Task
import Time exposing (Posix, Zone)
import Types exposing (Language(..), Translations)
import Url exposing (Url)
import Utils.PersistantState as PersistantState
import Utils.DateFormatter as DF


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        }


type alias Model =
    { appState : AppState
    , navKey : Browser.Navigation.Key
    , url : Url
    }


type alias Flags =
    { currentTime : Int
    , storage : Decode.Value
    }


type AppState
    = NotReady (Maybe Posix) (Maybe Zone) (Maybe PersistantState.State)
    | Ready SharedState Router.Model
    | FailedToInitialize


type Msg
    = UrlChange Url
    | LinkClicked UrlRequest
    | TimeChange Posix
    | SpinnerMsg Spinner.Msg
    | HandleTranslationsResponse (WebData Translations)
    | RouterMsg Router.Msg
    | AdjustTimeZone Zone
    | PersistanceUpdate (Maybe PersistantState.State)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { appState =
            NotReady
                (Just (Time.millisToPosix flags.currentTime))
                (Just Time.utc)
                (PersistantState.decode flags.storage)
      , url = url
      , navKey = navKey
      }
    , Cmd.batch
        [ Http.get
            { url = "/translations/de.json"
            , expect = Http.expectJson (RemoteData.fromResult >> HandleTranslationsResponse) Decoders.decodeTranslations
            }
        , Task.perform AdjustTimeZone Time.here
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeChange time ->
            updateTime model time

        AdjustTimeZone zone ->          
            updateTimeZone model zone

        HandleTranslationsResponse webData ->
            updateTranslations model webData

        UrlChange url ->
            updateRouter { model | url = url } (Router.UrlChange url)

        RouterMsg routerMsg ->
            updateRouter model routerMsg

        SpinnerMsg spinnerMsg ->
            updateRouter model (Router.SpinnerMsg spinnerMsg)

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                External url ->
                    ( model, Browser.Navigation.load url )

        PersistanceUpdate state ->
            updateRouter model <| Router.PersistanceUpdate state


updateTime : Model -> Posix -> ( Model, Cmd Msg )
updateTime model time =
    case model.appState of
        NotReady _ zone state ->
            ( { model | appState = NotReady (Just time) zone state }
            , Cmd.none
            )

        Ready sharedState routerModel ->
            ( { model | appState = Ready (SharedState.update sharedState (UpdateTime time)) routerModel }
            , Cmd.none
            )

        FailedToInitialize ->
            ( model, Cmd.none )


updateTimeZone : Model -> Zone -> ( Model, Cmd Msg )
updateTimeZone model zone =
    case model.appState of
        NotReady time _ state ->
            ( { model | appState = NotReady time (Just zone) state }
            , Cmd.none
            )

        Ready sharedState routerModel ->
            ( { model | appState = Ready (SharedState.update sharedState (UpdateTimezone zone)) routerModel }
            , Cmd.none
            )

        FailedToInitialize ->
            ( model, Cmd.none )


updateRouter : Model -> Router.Msg -> ( Model, Cmd Msg )
updateRouter model routerMsg =
    case model.appState of
        Ready sharedState routerModel ->
            let
                nextSharedState =
                    SharedState.update sharedState sharedStateUpdate

                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Router.update sharedState routerMsg routerModel
            in
            ( { model | appState = Ready nextSharedState nextRouterModel }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            let
                _ =
                    Debug.log "We got a router message even though the app is not ready?"
                        routerMsg
            in
            ( model, Cmd.none )


{-| Translations are the prerequisite for moving from `NotReady` to `Ready`.
This function has all the related logic.
-}
updateTranslations : Model -> WebData Translations -> ( Model, Cmd Msg )
updateTranslations model webData =
    case webData of
        -- If the initial request fails, we simply go to a failure state. In a real application, this case could be handled with e.g. retrying or using a built-in fallback value.
        Failure _ ->
            ( { model | appState = FailedToInitialize }, Cmd.none )

        -- If the translations were successfully loaded, we either:
        --   a) initialize the whole thing, or
        --   b) update the current running application.
        Success translations ->
            case model.appState of
                NotReady time zone state ->
                    -- We don't have a ready app, let's create one now
                    let
                        ( role, mail ) =
                            case state of
                                Just (PersistantState.State r m) ->
                                    ( Just r, Just m )

                                Nothing ->
                                    ( Nothing, Nothing )

                        initSharedState =
                            { navKey = model.navKey
                            , currentTime = time
                            , timezone = zone
                            , translations = translations
                            , selectedLanguage = German
                            , role = role
                            , userMail = mail
                            }

                        ( initRouterModel, routerCmd ) =
                            Router.init model.url initSharedState.selectedLanguage
                    in
                    ( { model | appState = Ready initSharedState initRouterModel }
                    , Cmd.map RouterMsg routerCmd
                    )

                Ready sharedState routerModel ->
                    -- If we do have an app ready, let's update the sharedState while keeping the routerModel unchanged.
                    ( { model
                        | appState =
                            Ready
                                (SharedState.update sharedState (UpdateLanguage German translations))
                                routerModel
                      }
                    , Cmd.none
                    )

                FailedToInitialize ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 TimeChange
        , Sub.map SpinnerMsg Spinner.subscription
        , PersistantState.storageToState PersistanceUpdate
        ]


view : Model -> Browser.Document Msg
view model =
    case model.appState of
        Ready sharedState routerModel ->
            Router.view RouterMsg sharedState routerModel

        NotReady _ _ _ ->
            { title = "Loading"
            , body = [ text "Loading" ]
            }

        FailedToInitialize ->
            { title = "Failure"
            , body = [ text "The application failed to initialize. " ]
            }



-- TODO: Style everything!
