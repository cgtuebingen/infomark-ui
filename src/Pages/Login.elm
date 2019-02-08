module Pages.Login exposing(Model, Msg(..), initModel, update, view)

import Browser.Navigation exposing (pushUrl)
import Decoders
import Dict
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import RemoteData exposing (RemoteData(..), WebData)
import Validate exposing (Validator, ifBlank, validate)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import I18n
import Time
import Types exposing (Language(..), Translations)
import Utils.Styles as Styles
import Utils.Icons as Icons
import Api.Data.Account exposing (Account)
import Api.Data.Role exposing (Role)
import Api.Request.Auth exposing (sessionPost)
import Spinner
import Svg 
import Svg.Attributes as SvgAttr


type alias Model =
    { email : String
    , plain_password : String
    , loginProgress : WebData Role
    , errors : (List Error)
    , spinner : Spinner.Model
    }

initModel : Model
initModel =
    { email = ""
    , plain_password = ""
    , loginProgress = NotAsked
    , errors = []
    , spinner = Spinner.init
    }

type Msg
    = NavigateTo Route
    | Login
    | SpinnerMsg Spinner.Msg
    | SetField Field String
    | LoginResponse (WebData Role) -- TODO: more like Tokens. Save tokens to shared state


type Field
    = Email | Password

setField : Model -> Field -> String -> Model
setField model field value = 
    let
        _ =
            Debug.log "SetField Called"
                value
    in
    case field of
        Email ->
            { model | email = value }
        
        Password ->
            { model | plain_password = value }

type alias Error =
    (Field, String)


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .email (Email, "Bitte gib deine E-Mail ein.")
        , ifBlank .plain_password (Password, "Bitte gib dein Passwort ein.")
        ]


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate)

        SpinnerMsg spinmsg ->
            let
                spinnerModel =
                    Spinner.update spinmsg model.spinner
            in
                ({ model | spinner = spinnerModel }, Cmd.none, NoUpdate)

        Login ->
            case validate modelValidator model of
                Err errors ->
                    ( {model | errors = errors}, Cmd.none, NoUpdate)

                Ok _ -> 
                    let
                        account = { email = model.email, plain_password = model.plain_password }
                    in
                    ( {model | loginProgress = Loading, errors = []}, sessionPost account LoginResponse, NoUpdate) -- TODO: Start the web request here.


        LoginResponse (RemoteData.Failure err) ->
            ({model | loginProgress = RemoteData.Failure err}, Cmd.none, NoUpdate)

        LoginResponse (RemoteData.Success role) ->
            ({model | loginProgress = RemoteData.NotAsked}, pushUrl sharedState.navKey (reverseRoute HomeRoute), UpdateRole <| Just role)

        LoginResponse response -> -- TODO show errors
            (model, Cmd.none, NoUpdate) -- TODO: Update the shared state




type alias LoginBody =
    { email : String
    , plain_password : String
    }


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        t =
            I18n.get sharedState.translations
    in
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
        [ parseWebDataForLoginView model.loginProgress
        , div
            [ classes
                [ TC.v_mid
                , TC.dtc 
                , TC.tc
                , TC.ph3 
                , TC.ph4_l] -- Center on parent
            ]
            [ div 
                [ classes 
                    [ TC.w3
                    , TC.dib
                    , TC.mt4
                    ]
                ]
                [ img [src "/assets/Logo.svg"] []]
            , Html.form 
                [ classes 
                    [ TC.mw7
                    , TC.center
                    , TC.pa4
                    , TC.black_40
                    ]
                , onSubmit Login
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
                            , TC.mb2
                            ]
                        , Styles.headerStyle
                        ]
                        [ text (t "page-title-login")] -- TODO: Replace with translation
                    , div [ classes[ TC.mt3 ] ]
                         <| inputElement "Email address" "Email" "email" Email model.email model.errors
                    , div [ classes[ TC.mt3 ] ]
                        <| inputElement "Passwort" "Password" "password" Password model.plain_password model.errors
                    , viewLoginButtonOrSpinner model.loginProgress model
                    ]
                    , div [ classes [ TC.mt3 ]]
                    [ button [ Styles.linkGreyStyle ] [ text "Passwort vergessen?" ] -- TODO: Create password reset page
                    , button [ onClick <| NavigateTo RegistrationRoute, Styles.linkGreyStyle ] [ text "Registrieren" ]
                    ]
                ]   
            ] 
        ] 

viewLoginButtonOrSpinner : WebData a -> Model -> Html Msg
viewLoginButtonOrSpinner status model =
    case status of
        RemoteData.Loading ->
            div [ 
                classes 
                    [ TC.dib
                    , TC.relative
                    , TC.w_100
                    , TC.mt5
                    , TC.mb3 
                    ]
                ] [Spinner.view Styles.spinnerRedStyle model.spinner]

        _ ->
            button 
                [ Styles.buttonGreyStyle
                , classes[TC.mt4, TC.w_100]
                ]
                [ text "Anmelden"] -- TODO: Replace with translation
            
parseWebDataForLoginView : WebData a -> Html Msg
parseWebDataForLoginView data =
    case data of
        RemoteData.Failure (Http.BadStatus 400) -> 
            viewLoginError "Wrong E-Mail and/or Password." -- TODO replace with translation

        RemoteData.Failure _ ->
            viewLoginError "Something went wrong" -- TODO replace with HELPFUL translation
        
        _ ->
            text ""

viewLoginError : String -> Html Msg
viewLoginError error =
    div 
        [ classes 
            [ TC.items_center
            , TC.justify_center
            , TC.w_100
            , TC.bg_red
            , TC.white
            , TC.flex
            , TC.pa4
            , TC.absolute
            ]
        , Styles.textStyle
        ]
        [ span [ classes [TC.mr2 ]] [Svg.svg [SvgAttr.width "22", SvgAttr.height "22", SvgAttr.viewBox "0 0 22 22"][Icons.warning Icons.white]]
        , text error
        ]

inputElement : String -> String -> String -> Field -> String -> List Error -> List (Html Msg)
inputElement inputLabel inputPlaceholder fieldType field curVal errors =
    [ label 
        [ classes [ TC.db, TC.lh_copy, TC.mb1 ]
        , Styles.labelStyle ]
        [ text inputLabel
        ] 
        , input [ type_ fieldType
                , Styles.inputStyle
                , classes [TC.w_100]
                , placeholder inputPlaceholder
                , onInput <| SetField field
                , value curVal
                ] []
        , viewFormErrors field errors
    ]

viewFormErrors : Field -> List Error -> Html Msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [TC.red]] [ text error ])
        |> ul [ classes [TC.list, TC.pl0, TC.center] ]
