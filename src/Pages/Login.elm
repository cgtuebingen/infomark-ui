module Pages.Login exposing(Model, Msg, initModel, update, view)

import Browser.Navigation exposing (pushUrl)
import Decoders
import Dict
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Time
import Types exposing (Language(..), Translations)
import Utils.Styles as Styles


type alias Model =
    { username : String
    , plain_password : String
    }

type Msg
    = NavigateTo Route


initModel : Model
initModel =
    { username = ""
    , plain_password = ""
    }

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div 
        [ classes
            [ TC.min_vh_100
            , TC.overflow_hidden
            , TC.db
            , TC.relative
            , TC.pb6
            , TC.dt
            , TC.w_100
            ] -- Fill nearly everythin
        ] 
        [ div
            [ classes
                [ TC.v_mid
                , TC.dtc 
                , TC.tc
                , TC.ph3 
                , TC.ph4_l] -- Center on parent
            ]
            [ Html.form 
                [ classes 
                    [ TC.bg_white
                    , TC.mw7
                    , TC.center
                    , TC.pa4
                    , TC.black_40
                    ]
                ]
                [ fieldset 
                    [ classes
                        [ TC.tl
                        , TC.bn
                        ]
                    ]
                    [ legend 
                        [ classes 
                            [ TC.pa0 
                            , TC.f2 
                            , TC.f3_ns
                            , TC.b 
                            , TC.mb2
                            , TC.black_80
                            ]
                        ]
                        [ text "Anmelden" ] -- Replace with translation
                    , div [ classes[ TC.mt3 ] ]
                        [
                            label [ classes [ TC.fw6, TC.f6, TC.db, TC.lh_copy ] ]
                                [ text "Email address" -- Replace with translation
                                ] 
                            , input [ type_ "text", name "email", 
                                    Styles.inputStyle,
                                    classes [TC.w_100]
                                    ] [] -- Add update function
                        ]
                    , div [ classes[ TC.mt3 ] ]
                        [
                            label [ classes [ TC.fw6, TC.f6, TC.db, TC.lh_copy ] ]
                                [ text "Passwort" -- Replace with translation
                                ] 
                            , input [ type_ "password", name "password", 
                                    Styles.inputStyle,
                                    classes [TC.w_70]
                                    ] [] -- Add update function
                            , button [ Styles.buttonRedStyle, classes [TC.w_20, TC.right_0] ] [ text "Anmelden"] -- Replace with translation
                        ]
                    ]
                ]   
            ] 
        ] 