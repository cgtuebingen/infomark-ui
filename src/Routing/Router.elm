module Routing.Router exposing (..)
--(Model, Msg(..), init, pageView, update, updateHome, updateSettings, view)

import Decoders
import Browser
import Browser.Navigation exposing (Key)
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import I18n
import Pages.Home as Home
import Pages.Login as Login
import Pages.Registration as Registration
import Pages.Courses as Courses
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Types exposing (Language(..), Translations)
import Url exposing (Url)
import Spinner
import Utils.Styles as Styles


type alias Model = 
    { homeModel : Home.Model
    , loginModel : Login.Model
    , registrationModel : Registration.Model
    , coursesModel : Courses.Model
    , route : Route
    , selectedLanguage : Language
    }


type Msg
    = UrlChange Url
    | NavigateTo Route
    | SelectedLanguage Language
    | HandleTranslationsResponse (WebData Translations)
    | SpinnerMsg Spinner.Msg
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | CoursesMsg Courses.Msg
    | RegistrationMsg Registration.Msg


init : Url -> Language -> ( Model, Cmd Msg )
init url lang =
    let 
        ( homeModel, homeCmd ) =
            Home.init

        loginModel =
            Login.initModel
        
        registrationModel =
            Registration.initModel

        ( coursesModel, coursesCmd ) =
            Courses.init
    in
    ( { homeModel = homeModel 
      , loginModel = loginModel
      , registrationModel = registrationModel
      , coursesModel = coursesModel
      , route = parseUrl url
      , selectedLanguage = lang
      }
    , Cmd.batch 
        [ Cmd.map HomeMsg homeCmd
        , Cmd.map CoursesMsg coursesCmd
        ]
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

        SelectedLanguage lang ->
            ( { model | selectedLanguage = lang }
            , getTranslations lang
            , NoUpdate
            )

        HandleTranslationsResponse webData ->
            case Debug.log "TranslationReceived" webData of
                Success translations ->
                    ( model, Cmd.none, UpdateLanguage model.selectedLanguage translations )

                _ -> 
                    ( model, Cmd.none, NoUpdate )

        SpinnerMsg spinnerMsg ->
            case model.route of -- Check in which part we are and forward the spinner msg if needed
                LoginRoute -> updateLogin sharedState model (Login.SpinnerMsg spinnerMsg)
            
                _ -> (model, Cmd.none, NoUpdate)

        HomeMsg homeMsg ->
            updateHome sharedState model homeMsg

        LoginMsg loginMsg ->
            updateLogin sharedState model loginMsg

        RegistrationMsg registrationMsg ->
            updateRegistration sharedState model registrationMsg

        CoursesMsg coursesMsg ->
            updateCourse sharedState model coursesMsg


updateHome : SharedState -> Model -> Home.Msg -> (Model, Cmd Msg, SharedStateUpdate)
updateHome sharedState model homeMsg = 
    let
        (nextHomeModel, homeCmd, sharedStateUpdate) =
            Home.update sharedState homeMsg model.homeModel
    in
    ( {model | homeModel = nextHomeModel}
    , Cmd.map HomeMsg homeCmd
    , sharedStateUpdate)


updateCourse : SharedState -> Model -> Courses.Msg -> (Model, Cmd Msg, SharedStateUpdate)
updateCourse sharedState model coursesMsg = 
    let
        (nextCoursesModel, coursesCmd, sharedStateUpdate) =
            Courses.update sharedState coursesMsg model.coursesModel
    in
    ( {model | coursesModel = nextCoursesModel}
    , Cmd.map CoursesMsg coursesCmd
    , sharedStateUpdate)


updateLogin : SharedState -> Model -> Login.Msg -> (Model, Cmd Msg, SharedStateUpdate)
updateLogin sharedState model loginMsg = 
    let 
        (nextLoginModel, loginCmd, sharedStateUpdate) =
            Login.update sharedState loginMsg model.loginModel
    in
    ( { model | loginModel = nextLoginModel}
    , Cmd.map LoginMsg loginCmd
    , sharedStateUpdate)


updateRegistration : SharedState -> Model -> Registration.Msg -> (Model, Cmd Msg, SharedStateUpdate)
updateRegistration sharedState model registrationMsg = 
    let 
        (nextRegistrationModel, registrationCmd, sharedStateUpdate) =
            Registration.update sharedState registrationMsg model.registrationModel
    in
    ( { model | registrationModel = nextRegistrationModel}
    , Cmd.map RegistrationMsg registrationCmd
    , sharedStateUpdate)


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
                    "Registration" -- TODO: Get translation from file
            
                HomeRoute ->
                    "Home"
                
                CoursesRoute ->
                    "Courses"
                
                NotFoundRoute ->
                    "404"
        
        body = case model.route of
            LoginRoute ->
                noTabPage sharedState model

            RegistrationRoute ->
                noTabPage sharedState model

            HomeRoute ->
                tabPage sharedState model

            CoursesRoute ->
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

navView : SharedState -> Model -> Html Msg
navView sharedState model = 
    nav 
        [ classes 
            [ TC.w_100
            , TC.flex
            , TC.justify_between
            , TC.items_center
            , TC.bb
            , TC.b__white_10
            , TC.bg_dark_red
            ]
        ]
        [input 
            [ type_ "image"
            , src "/assets/Logo_white.svg"
            , onClick <| NavigateTo HomeRoute
            , classes 
                [ TC.link
                , TC.pointer
                , TC.no_underline
                , TC.flex
                , TC.items_center
                , TC.pa3
                , TC.w2
                , TC.h2
                , TC.dim
                ]
            ] []
        , div 
            [ classes 
                [ TC.flex
                , TC.pa3
                ]
            ]
            [ button [ Styles.linkWhiteStyle, classes [TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu] ] [text "Courses"] -- TODO use translations
            , button [ Styles.linkWhiteStyle, classes [TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu] ] [text "Admin"] -- TODO use Translations - Only show if root
            , button [ Styles.linkWhiteStyle, classes [TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu] ] [text "Logout"]
            ]
        ]

footerView : SharedState -> Model -> Html Msg
footerView sharedState model =
    let
        t = 
            I18n.get sharedState.translations
    in
    footer 
        [ classes 
            [ TC.pv3
            , TC.ph3
            , TC.ph5_m
            , TC.ph6_l
            , TC.dark_red
            , TC.w_100
            , TC.db
            ]
        ]
        [ small 
            [ classes 
                [ TC.db
                , TC.tc
                ]
            , Styles.textStyle
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
            [ button [ Styles.linkGreyStyle, onClick <| SelectedLanguage German ] [ text "Deutsch" ]
            , button [ Styles.linkGreyStyle, onClick <| SelectedLanguage English ] [ text "English" ]
            , a [ Styles.linkGreyStyle ] [ text "Terms of Use" ]
            ]
        ]

tabPage : SharedState -> Model -> Html Msg
tabPage sharedState model = 
    main_ 
        [ classes 
            [ TC.w_100
            , TC.bg_white
            , TC.black
            , TC.helvetica
            ]
        ] 
        [ navView sharedState model
        , pageView sharedState model 
        , footerView sharedState model
        ]


noTabPage : SharedState -> Model -> Html Msg
noTabPage sharedState model = 
        div 
            [ classes
                [ TC.w_100
                , TC.white
                , TC.helvetica
                ]
            ]
            [ pageView sharedState model 
            , footerView sharedState model
            ]
        

pageView : SharedState -> Model -> Html Msg
pageView sharedState model =
    case model.route of 
        HomeRoute ->
            Home.view sharedState model.homeModel
                |> Html.map HomeMsg

        CoursesRoute ->
            Courses.view sharedState model.coursesModel
                |> Html.map CoursesMsg
            
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