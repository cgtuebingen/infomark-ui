module Pages.PasswordReset exposing (Model, Msg(..), init, update, view)

import Api.Data.UpdatePassword exposing (UpdatePassword)
import Api.Request.Auth exposing (updatePasswordPost)
import Browser.Navigation exposing (pushUrl)
import Components.Toasty
import Decoders
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Spinner
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Toasty
import Types exposing (Language(..), Translations)
import Utils.Styles as Styles
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    { email : String
    , token : String
    , password : String
    , passwordRepeat : String
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    }


init : String -> String -> ( Model, Cmd Msg )
init email token =
    ( { email = email
      , token = token
      , password = ""
      , passwordRepeat = ""
      , errors = []
      , toasties = Toasty.initialState
      }
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | SetField Field String
    | SetPassword
    | SetPasswordResponse (WebData ())
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        SetPassword ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    ( { model | errors = [] }, updatePasswordPost (modelToUpdatePassword model) SetPasswordResponse, NoUpdate )

        -- TODO: Start the web request here.
        SetPasswordResponse (Success _) ->
            ( model, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        SetPasswordResponse (Failure err) ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> addToast (Components.Toasty.Error "Error" "There was a problem changing your password.")
            in
            ( newModel, newCmd, NoUpdate )

        SetPasswordResponse response ->
            ( model, Cmd.none, NoUpdate )

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )


modelToUpdatePassword : Model -> UpdatePassword
modelToUpdatePassword model =
    { email = model.email
    , resetPasswordToken = model.token
    , password = model.password
    }


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        t =
            I18n.get sharedState.translations
    in
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
        [ Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
        , div
            [ classes
                [ TC.v_mid
                , TC.dtc
                , TC.tc
                , TC.ph3
                , TC.ph4_l
                ]

            -- Center on parent
            ]
            [ div
                [ classes
                    [ TC.w3
                    , TC.dib
                    , TC.mv4
                    ]
                ]
                [ img [ src "/assets/Logo.svg" ] [] ]
            , Html.form
                [ classes
                    [ TC.mw7
                    , TC.center
                    , TC.pa4
                    , TC.black_40
                    ]
                , onSubmit SetPassword
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
                        [ text (t "page-title-reset") ]

                    -- TODO: Replace with translation
                    , div [ classes [ TC.mt4 ] ] <|
                        inputElement "Password" "Password" "password" Password model.password model.errors
                    , div [ classes [ TC.mt4 ] ] <|
                        inputElement "Password repeat" "Password" "password" PasswordRepeat model.passwordRepeat model.errors
                    , button
                        [ Styles.buttonGreyStyle
                        , classes [ TC.mt4, TC.w_100 ]
                        , onClick SetPassword
                        ]
                        [ text "Change Password" ]
                    ]
                , div [ classes [ TC.mt3 ] ]
                    [ button
                        [ Styles.linkGreyStyle
                        , onClick <| NavigateTo LoginRoute
                        ]
                        [ text "Ich erinnere mich doch" ]
                    ]
                ]
            ]
        ]


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


viewFormErrors : Field -> List Error -> Html Msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [ TC.red ] ] [ text error ])
        |> ul [ classes [ TC.list, TC.pl0, TC.center ] ]


type Field
    = Password
    | PasswordRepeat


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Password ->
            { model | password = value }

        PasswordRepeat ->
            { model | passwordRepeat = value }


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .password ( Password, "Bitte gib ein Passwort ein." ) -- TODO: Check if password is at least 7 characters long
            , Validate.ifTrue (\model -> String.length model.password < 7) ( Password, "Das Passwort muss mindestens 7 Zeichen lang sein." )
            , ifBlank .passwordRepeat ( PasswordRepeat, "Bitte gib dein Passwort erneut ein." )
            , Validate.ifFalse (\model -> model.password == model.passwordRepeat) ( Password, "Die Passwörter müssen identisch sein." )
            ]
        ]


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )
