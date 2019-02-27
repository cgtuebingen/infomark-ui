module Pages.MailConfirmation exposing (Model, Msg(..), init, update, view)

import Api.Data.MailConfirmation exposing (MailConfirmation)
import Api.Request.Auth as AuthRequests
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Utils.Styles as Styles


type Msg
    = ConfirmationResponse (WebData ())


type alias Model =
    { confirmationProgress : WebData ()
    }


init : String -> String -> ( Model, Cmd Msg )
init email token =
    let
        confirmation =
            { email = email, confirmationToken = token }
    in
    ( { confirmationProgress = Loading }, AuthRequests.confirmMailPost confirmation ConfirmationResponse )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        ConfirmationResponse (Success response) ->
            ( model, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        ConfirmationResponse response ->
            ( { model | confirmationProgress = response }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    case model.confirmationProgress of
        Failure err ->
            div
                [ classes
                    [ TC.dtc
                    , TC.v_mid
                    , TC.tc
                    , TC.dark_red
                    , TC.ph3
                    , TC.ph4_l
                    ]
                , style "height" "80vh"
                , style "width" "100vw"
                ]
                [ h1
                    [ classes
                        [ TC.f6
                        , TC.f2_m
                        , TC.f_subheadline_l
                        , TC.fw6
                        , TC.tc
                        ]
                    ]
                    [ text "There was a problem with the confirmation!" ]
                ]

        _ ->
            text ""
