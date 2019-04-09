module Pages.Terms exposing (Model, Msg(..), init, update, view)

import Api.Request.Terms as TermsRequest
import Components.CommonElements as CE
import Html exposing (..)
import Http
import Markdown as MD
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Utils.Styles as Styles


type Msg
    = TermsOfUseResponse (WebData TermsRequest.Terms)


type alias Model =
    { terms : String }


init : ( Model, Cmd Msg )
init =
    ( { terms = "" }
    , TermsRequest.termsOfUseGet TermsOfUseResponse
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        TermsOfUseResponse response ->
            case response of
                Success terms ->
                    ( { model | terms = terms.terms }
                    , Cmd.none
                    , NoUpdate
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div
        [ classes
            [ TC.db
            , TC.pv5_l
            , TC.pv3_m
            , TC.pv1
            , TC.dt
            , TC.w_100
            ]
        ]
        [ div
            [ classes
                [ TC.dtc
                , TC.tc
                , TC.ph3
                , TC.ph4_l
                ]
            ]
            [ CE.rContainer <|
                [ CE.rRow <|
                    [ MD.toHtml [ Styles.textStyle ] <| model.terms ]
                ]
            ]
        ]
