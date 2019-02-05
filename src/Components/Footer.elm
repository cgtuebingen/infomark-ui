module Components.Footer exposing(..)

import Decoders
import Dict
import Http
import Html exposing (..)
import Html.Attributes exposing (href)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import RemoteData exposing (RemoteData(..), WebData)
import I18n
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Types exposing (Language(..), Translations)
import Utils.UniversityStyle as UTStyle


type alias Model = 
    { selectedLanguage : Language }


type Msg 
    = SelectedLanguage Language
    | HandleTranslationsResponse (WebData Translations)


initModel : Model
initModel =
    { selectedLanguage = German 
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SelectedLanguage lang ->
            ( { model | selectedLanguage = lang }
            , getTranslations lang
            , NoUpdate
            )

        HandleTranslationsResponse webData ->
            case webData of
                Success translations ->
                    ( model, Cmd.none, UpdateTranslations translations )

                _ -> 
                    ( model, Cmd.none, NoUpdate )


getTranslations : Language -> Cmd Msg
getTranslations language =
    let
        url =
            case language of
                English ->
                    "/translations/en.json"

                German ->
                    "/translations/de.json"

    in
        Http.get 
            { url = url
            , expect = Http.expectJson (RemoteData.fromResult >> HandleTranslationsResponse) Decoders.decodeTranslations
            
        }


linkStyle = classes 
    [ TC.f6 
    , TC.dib 
    , TC.ph2 
    , TC.link 
    , TC.mid_gray 
    , TC.dim 
    ]

view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        t = 
            I18n.get sharedState.translations
    in
    footer 
        [ classes 
            [ TC.pv4
            , TC.ph3
            , TC.ph5_m
            , TC.ph6_l
            , TC.mid_gray
            ]
        ]
        [ tachyons.css
        , small 
            [ classes 
                [ TC.f6
                , TC.db
                , TC.tc
                ]
            ]
            [ text "© 2019 "
                , b [ classes [TC.ttu] ]
                    [ text "University Tübingen" ]
                , text "., All Rights Reserved"
            ]
        , div
            [ classes
                [ TC.tc
                , TC.mt3
                ]
            ]
            [ a [ href "#0", linkStyle ] [ text "Deutsch" ]
            , a [ href "#0", linkStyle ] [ text "English" ]
            , a [ href "#0", linkStyle ] [ text "Terms of Use" ]
            ]
        ]