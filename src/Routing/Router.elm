module Routing.Router exposing (CurrentModel(..), Model, Msg(..), footerView, getTranslations, init, initWith, navView, navigateTo, noTabPage, pageView, tabPage, update, updateWith, view)

--(Model, Msg(..), init, pageView, update, updateHome, updateSettings, view)

import Browser
import Browser.Navigation exposing (Key)
import Decoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import I18n
import Pages.Admin as Admin
import Pages.CourseDetail as CourseDetail
import Pages.CourseEditor as CourseEditor
import Pages.Courses as Courses
import Pages.Dashboard as Dashboard
import Pages.Login as Login
import Pages.Registration as Registration
import Pages.SheetDetail as SheetDetail
import Pages.SheetEditor as SheetEditor
import Pages.SubmissionGradingEditor as SubmissionGradingEditor
import Pages.TaskEditor as TaskEditor
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Spinner
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Types exposing (Language(..), Translations)
import Url exposing (Url)
import Utils.Styles as Styles
import Utils.Utils as Utils
import Validate exposing (Validator, ifBlank, validate)
import Components.Dialog as Dialog
import Api.Data.Role exposing (Role)
import Api.Data.Account exposing (Account)
import Api.Request.Auth as AuthRequests


type alias Model =
    { currentModel : CurrentModel
    , route : Route
    , selectedLanguage : Language
    , loginDialogState : Dialog.State
    , email : String -- Only used for the modal refresh login dialog
    , plain_password : String -- Only used for the modal refresh login dialog
    , errors : List Error
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
    | LoginDialogShown Bool
    | SetField Field String
    | Login
    | LoginResponse (WebData Role)
    | Logout
    | LogoutCompleted (WebData ())
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
    | NoOp


init : Url -> Language -> ( Model, Cmd Msg )
init url lang =
    let
        currentRoute =
            parseUrl url
    in
    ( { currentModel = NotFound
      , route = currentRoute
      , selectedLanguage = lang
      , loginDialogState = False
      , email = ""
      , plain_password = ""
      , errors = []
      }
    , Utils.perform <| UrlChange url
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case ( msg, model.currentModel ) of
        ( UrlChange location, _ ) ->
            let
                route =
                    parseUrl location

                ( newModel, newCmd, newSharedStateUpdate ) =
                    navigateTo route model
            in
            ( { newModel | route = route }
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
            case webData of
                Success translations ->
                    ( model, Cmd.none, UpdateLanguage model.selectedLanguage translations )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ( Logout, _ ) ->
            ( model, AuthRequests.sessionDelete LogoutCompleted, NoUpdate )

        ( LogoutCompleted (Success _), _ ) -> -- Go back to login
            ( model, Browser.Navigation.pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate)

        ( LogoutCompleted _, _) -> -- Failure, Not asked or Loading
            ( model, Cmd.none, NoUpdate)

        ( SpinnerMsg spinnerMsg, LoginModel login ) ->
            Login.update sharedState (Login.SpinnerMsg spinnerMsg) login
                |> updateWith LoginModel LoginMsg model

        ( LoginMsg loginMsg, LoginModel login ) ->
            Login.update sharedState loginMsg login
                |> updateWith LoginModel LoginMsg model

        ( RegistrationMsg registrationMsg, RegistrationModel registration ) ->
            Registration.update sharedState registrationMsg registration
                |> updateWith RegistrationModel RegistrationMsg model

        ( DashboardMsg dashboardMsg, DashboardModel dashboard ) ->
            Dashboard.update sharedState dashboardMsg dashboard
                |> updateWith DashboardModel DashboardMsg model

        ( CoursesMsg coursesMsg, CoursesModel courses ) ->
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

        ( SubmissionGradingEditorMsg submissionEditorMsg, SubmissionGradingEditorModel submissionEditor ) ->
            SubmissionGradingEditor.update sharedState submissionEditorMsg submissionEditor
                |> updateWith SubmissionGradingEditorModel SubmissionGradingEditorMsg model

        ( AdminMsg adminMsg, AdminModel admin ) ->
            Admin.update sharedState adminMsg admin
                |> updateWith AdminModel AdminMsg model

        ( LoginDialogShown state, _) ->
            ( { model | loginDialogState = state }, Cmd.none, NoUpdate )

        ( SetField field val, _ ) ->
            let
                newModel = setField model field val
            in
            ( newModel, Cmd.none, NoUpdate )

        ( Login, _ ) ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    ( model
                    , AuthRequests.sessionPost (modelToLoginRequest sharedState model) LoginResponse
                    , NoUpdate)

        ( LoginResponse (Failure err), _ ) -> -- Failure. Show Toast
            ( model, Cmd.none, NoUpdate )

        ( LoginResponse (Success role), _ ) -> -- Success. Hide the dialog again
            ( {model | loginDialogState = False }, Cmd.none, UpdateRoleAndMail role model.email )

        ( _, _ ) ->
            -- Message arrived for wrong page. Ignore that
            ( model, Cmd.none, NoUpdate )


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
        t =
            I18n.get sharedState.translations

        title =
            case model.route of
                LoginRoute ->
                    "page-title-login"

                RegistrationRoute ->
                    "page-title-registration"

                DashboardRoute ->
                    "page-title-dashboard"

                CoursesRoute ->
                    "page-title-courses"

                CreateCourseRoute ->
                    "page-title-create-course"

                EditCourseRoute _ ->
                    "page-title-edit-course"

                CourseDetailRoute _ ->
                    "page-title-course"

                CreateSheetRoute ->
                    "page-title-create-sheet"

                EditSheetRoute _ ->
                    "page-title-edit-sheet"

                SheetDetailRoute _ ->
                    "page-title-sheet"

                CreateTaskRoute ->
                    "page-title-create-task"

                EditTaskRoute _ ->
                    "page-title-edit-task"

                SubmissionGradingRoute _ _ ->
                    "page-title-grade"

                AdminRoute ->
                    "page-title-admin"

                NotFoundRoute ->
                    "page-title-404"

        body =
            case model.route of
                LoginRoute ->
                    noTabPage sharedState model

                RegistrationRoute ->
                    noTabPage sharedState model

                NotFoundRoute ->
                    noTabPage sharedState model

                _ ->
                    tabPage sharedState model
    in
    { title = t "site-title" ++ " - " ++ t title
    , body =
        [ body
            |> Html.map msgMapper
        ]
    }


navView : SharedState -> Model -> Html Msg
navView sharedState model =
    let
        t =
            I18n.get sharedState.translations
    in
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
        [ input
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
                , TC.ml1
                , TC.ml4_ns
                ]
            ]
            []
        , div
            [ classes
                [ TC.flex
                , TC.pa3
                ]
            ]
            [ button
                [ Styles.linkWhiteStyle
                , classes [ TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu ]
                , onClick <| NavigateTo CoursesRoute
                ]
                [ text (t "page-title-courses") ]

            -- TODO use translations
            , button
                [ Styles.linkWhiteStyle
                , classes [ TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu ]
                , onClick <| NavigateTo AdminRoute
                ]
                [ text (t "page-title-admin") ]

            -- TODO use Translations - Only show if root
            , button
                [ Styles.linkWhiteStyle
                , classes [ TC.mr1, TC.mr4_ns, TC.fw6, TC.tracked, TC.ttu ]
                , onClick Logout
                ]
                [ text (t "action-logout") ]
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
            , b [ classes [ TC.ttu ] ]
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
        [ loginDialog sharedState model
        , navView sharedState model
        , pageView sharedState model
        , footerView sharedState model
        ]


loginDialog : SharedState -> Model -> Html Msg
loginDialog sharedState model =
    let
        inputs = case sharedState.userMail of
            Just mail -> -- We already got the mail. Only let the user 
                         -- enter the password
                inputElement "Password" "Password" "password" Password model.plain_password model.errors

            Nothing -> -- We lost the mail. Ask the user again
                (inputElement "Email" "Email" "email" Email model.email model.errors) ++
                (inputElement "Password" "Password" "password" Password model.plain_password model.errors)
    in
    Dialog.modalDialog div 
        [ classes
            [ TC.fixed
            , TC.top_0
            , TC.right_0
            , TC.bottom_0
            , TC.left_0
            , TC.bg_black_30
            , TC.justify_center
            , TC.items_center
            ]
        ]
        ( Dialog.dialog div
            [ classes
                [ TC.bg_white
                , TC.black
                , TC.ba
                , TC.bw1
                , TC.b__black_60
                , TC.shadow_5
                , TC.w_100
                , TC.measure_wide
                , TC.ph5
                , TC.pv4
                ]
            ]
            [ div 
                [ classes [TC.w_100, TC.ph1, TC.bb, TC.bw2, TC.b__black] ] 
                [ h1 [] [text "Are you still there?"] ]
            , div
                [ classes [ TC.w_100, TC.mt4 ]]
                [ Html.form 
                    [] 
                    (inputs ++
                    [ div [ classes [ TC.fr, TC.mt3 ] ]
                        [ button 
                            [ classes 
                                []
                            , Styles.buttonRedStyle
                            , onClick <| Logout
                            ] [ text "Nah. I'm away"]    
                        , button 
                            [ classes 
                                [ TC.ml3 ]
                            , Styles.buttonGreenStyle
                            , onClick Login -- TODO perform the login
                            ] [ text "Login Again" ]
                        ]
                    ])
                ]
                
            ]
        )
        model.loginDialogState
        loginDialogConfig
        

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
                    [ text "404 :(" ]
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


updateWith : (subModel -> CurrentModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg, SharedStateUpdate ) -> ( Model, Cmd Msg, SharedStateUpdate )
updateWith toModel toMsg model ( subModel, subCmd, subSharedStateUpdate ) =
    let
        (newModel, newSharedState) = 
            if subSharedStateUpdate == RefreshLogin then -- Intercept the request if a login is needed again
                ({ model | loginDialogState = True }, NoUpdate)
            else
                (model, subSharedStateUpdate) 
    in
    ( { newModel | currentModel = toModel subModel }
    , Cmd.map toMsg subCmd
    , newSharedState
    )


initWith : (subModel -> CurrentModel) -> (subMsg -> Msg) -> Model -> SharedStateUpdate -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg, SharedStateUpdate )
initWith toModel toMsg model sharedStateUpdate ( subModel, subCmd ) =
    ( { model | currentModel = toModel subModel }
    , Cmd.map toMsg subCmd
    , sharedStateUpdate
    )


type Field
    = Email
    | Password


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Email ->
            { model | email = value }

        Password ->
            { model | plain_password = value }


type alias Error =
    ( Field, String )


inputElement : String -> String -> String -> Field -> String -> List Error -> List (Html Msg)
inputElement inputLabel inputPlaceholder fieldType field curVal errors =
    [ label
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle
        ]
        [ text inputLabel
        ]
    , input
        [ type_ fieldType
        , Styles.lineInputStyle
        , classes [ TC.w_100, TC.mb3 ]
        , placeholder inputPlaceholder
        , onInput <| SetField field
        , value curVal
        ]
        []
    , viewFormErrors field errors
    ]


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .email ( Email, "Bitte gib deine E-Mail ein." )
        , ifBlank .plain_password ( Password, "Bitte gib dein Passwort ein." )
        ]


viewFormErrors : Field -> List Error -> Html Msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [ TC.red ] ] [ text error ])
        |> ul [ classes [ TC.list, TC.pl0, TC.center ] ]


modelToLoginRequest : SharedState -> Model -> Account
modelToLoginRequest sharedState model =
    case sharedState.userMail of
        Just mail ->
            { email = mail
            , plain_password = model.plain_password 
            }

        Nothing ->
            { email = model.email
            , plain_password = model.plain_password 
            }

loginDialogConfig : Dialog.Config Msg
loginDialogConfig =
    Dialog.Config
        (classes [ TC.flex ])
        (classes [ TC.dn])
        LoginDialogShown
        False
        NoOp