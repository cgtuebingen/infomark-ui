module Routing.Router exposing (..)
--(Model, Msg(..), init, pageView, update, updateHome, updateSettings, view)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import I18n
import Components.Footer as Footer
import Pages.Home as Home
import Pages.Login as Login
import Pages.Registration as Registration
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Types exposing (Translations)
import Url exposing (Url)


type alias Model = 
    { homeModel : Home.Model
    , loginModel : Login.Model
    , registrationModel : Registration.Model
    , footerModel : Footer.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | NavigateTo Route
    | FooterMsg Footer.Msg
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegistrationMsg Registration.Msg


init : Url -> ( Model, Cmd Msg )
init url =
    let 
        ( homeModel, homeCmd) =
            Home.init

        loginModel =
            Login.initModel
        
        registrationModel =
            Registration.initModel

        footerModel = 
            Footer.initModel
    in
    ( { homeModel = homeModel 
      , loginModel = loginModel
      , registrationModel = registrationModel
      , footerModel = footerModel
      , route = parseUrl url
      }
    , Cmd.map HomeMsg homeCmd
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UrlChange location ->
            ( { model | route = parseUrl location }
            , Cmd.none
            , NoUpdate
            )
        
        NavigateTo route ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        HomeMsg homeMsg ->
            (model, Cmd.none, NoUpdate)

        LoginMsg loginMsg ->
            (model, Cmd.none, NoUpdate)

        RegistrationMsg registrationMsg ->
            (model, Cmd.none, NoUpdate)

        FooterMsg footerMsg ->
            (model, Cmd.none, NoUpdate)


view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let 
        translate = 
            I18n.get sharedState.translations

        title =
            case model.route of
                LoginRoute ->
                    "Login"
                
                RegistrationRoute ->
                    "Registration" -- Get translation from file
            
                HomeRoute ->
                    "Home"
                
                NotFoundRoute ->
                    "404"
        
        body = case model.route of
            LoginRoute ->
                noTabPage sharedState model

            RegistrationRoute ->
                noTabPage sharedState model

            HomeRoute ->
                tabPage sharedState model

            NotFoundRoute ->
                tabPage sharedState model
    in
    { title = "InfoMark - " ++ title
    , body =
        [ body
            |> Html.map msgMapper
        ]
    }



tabPage : SharedState -> Model -> Html Msg
tabPage sharedState model = 
    main_ 
        [ classes 
            [ TC.vh_100
            , TC.dt
            , TC.w_100
            , TC.bg_near_white
            ]
        ] 
        [ text "Tabs"
        ]


noTabPage : SharedState -> Model -> Html Msg
noTabPage sharedState model = 
        div 
            [ classes
                [ TC.h_100
                , TC.relative
                , TC.bg_near_white
                ]
            ]
            [ pageView sharedState model 
            , Footer.view sharedState model.footerModel
                |> Html.map FooterMsg
            ]
        

pageView : SharedState -> Model -> Html Msg
pageView sharedState model =
    case model.route of 
        HomeRoute ->
            Home.view sharedState model.homeModel
                |> Html.map HomeMsg
            

        LoginRoute ->
            Login.view sharedState model.loginModel
                |> Html.map LoginMsg

        RegistrationRoute ->
            Registration.view sharedState model.registrationModel
                |> Html.map RegistrationMsg

        NotFoundRoute ->
            div 
                [ classes
                    [ TC.dtc
                    , TC.v_mid
                    , TC.tc
                    , TC.dark_red
                    , TC.ph3
                    , TC.ph4_l
                    ]
                ]
                [
                    h1 
                        [ classes 
                            [ TC.f6
                            , TC.f2_m
                            , TC.f_subheadline_l
                            , TC.fw6
                            , TC.tc
                            ]
                        ]
                        [ text "404 :("]
                ]