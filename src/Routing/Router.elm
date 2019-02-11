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
import Pages.Courses as Courses
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Types exposing (Translations)
import Url exposing (Url)
import Spinner


type alias Model = 
    { homeModel : Home.Model
    , loginModel : Login.Model
    , registrationModel : Registration.Model
    , coursesModel : Courses.Model
    , footerModel : Footer.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | NavigateTo Route
    | SpinnerMsg Spinner.Msg
    | FooterMsg Footer.Msg
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | CoursesMsg Courses.Msg
    | RegistrationMsg Registration.Msg


init : Url -> ( Model, Cmd Msg )
init url =
    let 
        ( homeModel, homeCmd ) =
            Home.init

        loginModel =
            Login.initModel
        
        registrationModel =
            Registration.initModel

        ( coursesModel, coursesCmd ) =
            Courses.init

        footerModel = 
            Footer.initModel
    in
    ( { homeModel = homeModel 
      , loginModel = loginModel
      , registrationModel = registrationModel
      , coursesModel = coursesModel
      , footerModel = footerModel
      , route = parseUrl url
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

        FooterMsg footerMsg ->
            updateFooter sharedState model footerMsg


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


updateFooter : SharedState -> Model -> Footer.Msg -> (Model, Cmd Msg, SharedStateUpdate)
updateFooter sharedState model footerMsg = 
    let 
        (nextFooterModel, footerCmd, sharedStateUpdate) =
            Footer.update sharedState footerMsg model.footerModel
    in
    ( { model | footerModel = nextFooterModel}
    , Cmd.map FooterMsg footerCmd
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



tabPage : SharedState -> Model -> Html Msg
tabPage sharedState model = 
    main_ 
        [ classes 
            [ TC.vh_100
            , TC.dt
            , TC.w_100
            , TC.bg_white
            , TC.black
            , TC.helvetica
            ]
        ] 
        [ pageView sharedState model 
        , Footer.view sharedState model.footerModel
            |> Html.map FooterMsg
        ]


noTabPage : SharedState -> Model -> Html Msg
noTabPage sharedState model = 
        div 
            [ classes
                [ TC.h_100
                , TC.relative
                , TC.white
                , TC.helvetica
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