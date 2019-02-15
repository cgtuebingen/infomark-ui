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
import Pages.Login as Login
import Pages.Registration as Registration
import Pages.Dashboard as Dashboard
import Pages.Courses as Courses
import Pages.CourseEditor as CourseEditor
import Pages.CourseDetail as CourseDetail
import Pages.SheetEditor as SheetEditor
import Pages.SheetDetail as SheetDetail
import Pages.TaskEditor as TaskEditor
import Pages.Admin as Admin
import Pages.SubmissionGradingEditor as SubmissionGradingEditor
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Types exposing (Language(..), Translations)
import Url exposing (Url)
import Spinner
import Utils.Styles as Styles
import Utils.Utils as Utils


type alias Model = 
    { currentModel : CurrentModel
    , route : Route
    , selectedLanguage : Language
    }

type CurrentModel
    = LoginModel Login.Model
    | RegistrationModel Registration.Model
    | DashboardModel Dashboard.Model
    | AdminModel Admin.Model
    | CoursesModel Courses.Model
    | CourseDetailModel CourseDetail.Model
    | CourseEditorModel CourseEditor.Model
    | SheetDetailModel SheetDetail.Model
    | SheetEditorModel SheetEditor.Model
    | SubmissionGradingEditorModel SubmissionGradingEditor.Model
    | TaskEditorModel TaskEditor.Model
    | NotFound


type Msg
    = UrlChange Url
    | NavigateTo Route
    | SelectedLanguage Language
    | Logout
    | HandleTranslationsResponse (WebData Translations)
    | SpinnerMsg Spinner.Msg
    | LoginMsg Login.Msg
    | RegistrationMsg Registration.Msg
    | DashboardMsg Dashboard.Msg
    | AdminMsg Admin.Msg
    | CoursesMsg Courses.Msg
    | CourseDetailMsg CourseDetail.Msg
    | CourseEditorMsg CourseEditor.Msg
    | SheetDetailMsg SheetDetail.Msg
    | SheetEditorMsg SheetEditor.Msg
    | SubmissionGradingEditorMsg SubmissionGradingEditor.Msg
    | TaskEditorMsg TaskEditor.Msg


init : Url -> Language -> ( Model, Cmd Msg )
init url lang =
    let
        currentRoute = parseUrl url
    in
    ( { currentModel = NotFound
      , route = currentRoute
      , selectedLanguage = lang
      }
    , Utils.perform <| UrlChange url
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case (msg, model.currentModel) of
        ( UrlChange location, _ ) ->
            let 
                route = parseUrl location
                (newModel, newCmd, newSharedStateUpdate) = navigateTo route model
            in
            ( {newModel | route = route }
            , newCmd
            , newSharedStateUpdate
            )

        ( NavigateTo route, _ ) ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        ( SelectedLanguage lang, _ ) ->
            ( { model | selectedLanguage = lang }
            , getTranslations lang
            , NoUpdate
            )

        ( HandleTranslationsResponse webData, _ ) ->
            case Debug.log "TranslationReceived" webData of
                Success translations ->
                    ( model, Cmd.none, UpdateLanguage model.selectedLanguage translations )

                _ -> 
                    ( model, Cmd.none, NoUpdate )

        ( Logout, _ ) ->
            (model, Cmd.none, NoUpdate) -- TODO send logout message

        ( SpinnerMsg spinnerMsg, LoginModel login ) ->
            Login.update sharedState (Login.SpinnerMsg spinnerMsg) login
                |> updateWith LoginModel LoginMsg model

        ( LoginMsg loginMsg, LoginModel login ) ->
            Login.update sharedState loginMsg login
                |> updateWith LoginModel LoginMsg model

        ( RegistrationMsg registrationMsg, RegistrationModel registration) ->
            Registration.update sharedState registrationMsg registration
                |> updateWith RegistrationModel RegistrationMsg model

        ( DashboardMsg dashboardMsg, DashboardModel dashboard) ->
            Dashboard.update sharedState dashboardMsg dashboard
                |> updateWith DashboardModel DashboardMsg model

        ( CoursesMsg coursesMsg, CoursesModel courses) ->
            Courses.update sharedState coursesMsg courses
                |> updateWith CoursesModel CoursesMsg model

        ( CourseEditorMsg courseEditorMsg, CourseEditorModel courseEditor ) ->
            CourseEditor.update sharedState courseEditorMsg courseEditor 
                |> updateWith CourseEditorModel CourseEditorMsg model

        ( CourseDetailMsg courseDetailMsg, CourseDetailModel courseDetail ) ->
            CourseDetail.update sharedState courseDetailMsg courseDetail
                |> updateWith CourseDetailModel CourseDetailMsg model

        ( SheetEditorMsg sheetEditorMsg, SheetEditorModel sheetEditor ) ->
            SheetEditor.update sharedState sheetEditorMsg sheetEditor
                |> updateWith SheetEditorModel SheetEditorMsg model

        ( SheetDetailMsg sheetDetailMsg, SheetDetailModel sheetDetail ) ->
            SheetDetail.update sharedState sheetDetailMsg sheetDetail
                |> updateWith SheetDetailModel SheetDetailMsg model

        ( TaskEditorMsg taskEditorMsg, TaskEditorModel taskEditor ) ->
            TaskEditor.update sharedState taskEditorMsg taskEditor
                |> updateWith TaskEditorModel TaskEditorMsg model

        ( SubmissionGradingEditorMsg submissionEditorMsg, SubmissionGradingEditorModel submissionEditor) ->
            SubmissionGradingEditor.update sharedState submissionEditorMsg submissionEditor
                |> updateWith SubmissionGradingEditorModel SubmissionGradingEditorMsg model

        ( AdminMsg adminMsg, AdminModel admin ) ->
            Admin.update sharedState adminMsg admin
                |> updateWith AdminModel AdminMsg model 

        ( _, _ ) ->
            -- Message arrived for wrong page. Ignore that
            (model, Cmd.none, NoUpdate) 

navigateTo : Route -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
navigateTo route model =
    case route of
        LoginRoute ->
            Login.init |> initWith LoginModel LoginMsg model NoUpdate

        RegistrationRoute ->
            Registration.init |> initWith RegistrationModel RegistrationMsg model NoUpdate
        
        DashboardRoute ->
            Dashboard.init |> initWith DashboardModel DashboardMsg model NoUpdate

        CoursesRoute ->
            Courses.init |> initWith CoursesModel CoursesMsg model NoUpdate

        CreateCourseRoute ->
            CourseEditor.initCreate |> initWith CourseEditorModel CourseEditorMsg model NoUpdate

        EditCourseRoute id ->
            CourseEditor.initEdit id |> initWith CourseEditorModel CourseEditorMsg model NoUpdate

        CourseDetailRoute id ->
            CourseDetail.init id |> initWith CourseDetailModel CourseDetailMsg model NoUpdate

        CreateSheetRoute ->
            SheetEditor.initCreate |> initWith SheetEditorModel SheetEditorMsg model NoUpdate

        EditSheetRoute id ->
            SheetEditor.initEdit id |> initWith SheetEditorModel SheetEditorMsg model NoUpdate

        SheetDetailRoute id ->
            SheetDetail.init id |> initWith SheetDetailModel SheetDetailMsg model NoUpdate

        CreateTaskRoute ->
            TaskEditor.initCreate |> initWith TaskEditorModel TaskEditorMsg model NoUpdate

        EditTaskRoute id ->
            TaskEditor.initEdit id |> initWith TaskEditorModel TaskEditorMsg model NoUpdate

        SubmissionGradingRoute taskId groupId ->
            SubmissionGradingEditor.init taskId groupId |> initWith SubmissionGradingEditorModel SubmissionGradingEditorMsg model NoUpdate

        AdminRoute ->
            Admin.init |> initWith AdminModel AdminMsg model NoUpdate

        NotFoundRoute ->
            ( { model | currentModel = NotFound }
            , Cmd.none
            , NoUpdate
            )

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
                
                CoursesRoute ->
                    "Courses"
                
                NotFoundRoute ->
                    "404"

                _ ->
                    "Placeholder"
        
        body = case model.route of
            LoginRoute ->
                noTabPage sharedState model

            RegistrationRoute ->
                noTabPage sharedState model

            _ ->
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
            , onClick <| NavigateTo DashboardRoute
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
            [ button 
                [ Styles.linkWhiteStyle
                , classes [TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu]
                , onClick <| NavigateTo CoursesRoute
                ] [text "Courses"] -- TODO use translations
            , button 
                [ Styles.linkWhiteStyle
                , classes [TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu] 
                , onClick <| NavigateTo AdminRoute
                ] [text "Admin"] -- TODO use Translations - Only show if root
            , button 
                [ Styles.linkWhiteStyle
                , classes [TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu]
                , onClick Logout 
                ] [text "Logout"]
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
    case model.currentModel of 
        LoginModel login ->
            Login.view sharedState login
                |> Html.map LoginMsg

        RegistrationModel registration ->
            Registration.view sharedState registration
                |> Html.map RegistrationMsg

        DashboardModel dashboard ->
            Dashboard.view sharedState dashboard
                |> Html.map DashboardMsg

        CoursesModel courses ->
            Courses.view sharedState courses
                |> Html.map CoursesMsg

        CourseEditorModel courseEditor ->
            CourseEditor.view sharedState courseEditor
                |> Html.map CourseEditorMsg

        CourseDetailModel courseDetail ->
            CourseDetail.view sharedState courseDetail
                |> Html.map CourseDetailMsg

        SheetEditorModel sheetEditor ->
            SheetEditor.view sharedState sheetEditor
                |> Html.map SheetEditorMsg

        SheetDetailModel sheetDetail ->
            SheetDetail.view sharedState sheetDetail
                |> Html.map SheetDetailMsg

        TaskEditorModel taskEditor ->
            TaskEditor.view sharedState taskEditor
                |> Html.map TaskEditorMsg

        SubmissionGradingEditorModel submissionEditor ->
            SubmissionGradingEditor.view sharedState submissionEditor
                |> Html.map SubmissionGradingEditorMsg
            
        AdminModel admin ->
            Admin.view sharedState admin
                |> Html.map AdminMsg

        NotFound ->
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

{-
modelForRoute : Route -> CurrentModel
modelForRoute route =
    case route of
        LoginRoute ->
            LoginModel <| Tuple.first Login.init

        RegistrationRoute ->
            RegistrationModel <| Tuple.first Registration.init

        DashboardRoute ->
            DashboardModel <| Tuple.first Dashboard.init

        CoursesRoute ->
            CoursesModel <| Tuple.first Courses.init

        CreateCourseRoute ->
            CourseEditorModel <| Tuple.first CourseEditor.initCreate

        EditCourseRoute id ->
            CourseEditorModel <| Tuple.first <| CourseEditor.initEdit id
            
        CourseDetailRoute id ->
            CourseDetailModel <| Tuple.first <| CourseDetail.init id

        CreateSheetRoute ->
            SheetEditorModel <| Tuple.first SheetEditor.initCreate

        EditSheetRoute id ->
            SheetEditorModel <| Tuple.first <| SheetEditor.initEdit id

        SheetDetailRoute id ->
            SheetDetailModel <| Tuple.first <| SheetDetail.init id

        CreateTaskRoute ->
            TaskEditorModel <| Tuple.first TaskEditor.initCreate

        EditTaskRoute id ->
            TaskEditorModel <| Tuple.first <| TaskEditor.initEdit id

        SubmissionGradingRoute taskId groupId ->
            SubmissionGradingEditorModel <| Tuple.first <| SubmissionGradingEditor.init taskId groupId

        AdminRoute ->
            AdminModel <| Tuple.first Admin.init

        _ ->
            NotFound
-}

updateWith : (subModel -> CurrentModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg, SharedStateUpdate) -> ( Model, Cmd Msg, SharedStateUpdate )
updateWith toModel toMsg model ( subModel, subCmd, subSharedStateUpdate )  =
    ( {model | currentModel = toModel subModel }
    , Cmd.map toMsg subCmd
    , subSharedStateUpdate
    )

initWith : (subModel -> CurrentModel) -> (subMsg -> Msg) -> Model -> SharedStateUpdate -> ( subModel, Cmd subMsg) -> ( Model, Cmd Msg, SharedStateUpdate )
initWith toModel toMsg model sharedStateUpdate ( subModel, subCmd ) =
    ( {model | currentModel = toModel subModel }
    , Cmd.map toMsg subCmd
    , sharedStateUpdate
    )