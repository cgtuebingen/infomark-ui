module Pages.Registration exposing (Error, Field(..), Model, Msg(..), init, inputElement, modelValidator, setField, update, view, viewFormErrors)

import Api.Data.User exposing (User)
import Api.Data.UserAccount exposing (UserAccount)
import Api.Request.Account exposing (accountPost)
import Browser.Navigation exposing (pushUrl)
import Components.Dialog as Dialog
import Components.Toasty
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Toasty
import Types exposing (Language(..), Translations, languageToBackendString)
import Utils.EmailHelper as UniMailChecker
import Utils.Styles as Styles
import Validate exposing (Validator, ifBlank, ifInvalidEmail, ifNotInt, validate)


type alias Model =
    { email : String
    , password : String
    , passwordRepeat : String
    , firstName : String
    , lastName : String
    , studentNumber : String
    , semester : String
    , subject : String
    , registrationProgress : WebData User
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    , noUniversityEmailDialogState : Dialog.State
    }


modelToBody : SharedState -> Model -> UserAccount
modelToBody sharedState model =
    { user =
        { id = 0
        , firstname = model.firstName
        , lastname = model.lastName
        , avatarUrl = Nothing
        , email = model.email
        , studentNumber = Just model.studentNumber
        , semester = String.toInt model.semester
        , language = Just <| languageToBackendString sharedState.selectedLanguage
        , subject = Just model.subject
        }
    , account =
        { email = model.email
        , plain_password = model.password
        }
    }


type Msg
    = NavigateTo Route
    | Register Bool
    | SetField Field String
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | RegistrationResponse (WebData User)
    | NoUniversityMailWarningVisible Bool
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
      , passwordRepeat = ""
      , firstName = ""
      , lastName = ""
      , studentNumber = ""
      , semester = ""
      , subject = ""
      , registrationProgress = NotAsked
      , errors = []
      , toasties = Toasty.initialState
      , noUniversityEmailDialogState = False
      }
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        Register force ->
            updateHandleRegister sharedState model force <| validate modelValidator model

        RegistrationResponse response ->
            updateHandleRegistrationResponse sharedState model response

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )

        NoUniversityMailWarningVisible state ->
            ( { model | noUniversityEmailDialogState = state }, Cmd.none, NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


updateHandleRegister : SharedState -> Model -> Bool -> Result (List Error) success -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleRegister sharedState model force validationResult =
    case validate modelValidator model of
        Err errors ->
            ( { model | errors = errors }, Cmd.none, NoUpdate )

        Ok _ ->
            case ( UniMailChecker.isInvalid model.email, force ) of
                ( True, False ) ->
                    -- Show a warning
                    ( { model | noUniversityEmailDialogState = True }, Cmd.none, NoUpdate )

                ( _, _ ) ->
                    -- In all other cases continue with registration
                    ( { model | registrationProgress = Loading, errors = [] }
                    , accountPost (modelToBody sharedState model) RegistrationResponse
                    , NoUpdate
                    )


updateHandleRegistrationResponse : SharedState -> Model -> WebData User -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleRegistrationResponse sharedState model response =
    case response of
        Success _ ->
            ( { model | registrationProgress = response }, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        Failure (Http.BadBody error) ->
            let
                _ =
                    Debug.log "Bad response:" error
            in
            ( { model | registrationProgress = response }, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        Failure err ->
            let
                ( newModel, newCmd ) =
                    ( { model | registrationProgress = response }, Cmd.none )
                        |> addToast
                            (Components.Toasty.Error "Error" "Failed to register")
            in
            ( newModel, newCmd, NoUpdate )

        _ ->
            ( { model | registrationProgress = response }, Cmd.none, NoUpdate )



-- TODO: Update the shared state
-- TODO notify user that he must confirm his email or talk to admins


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
        [ noUniEmailDialog sharedState model
        , Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
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
                , onSubmit <| Register False
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
                        [ text (t "page-title-registration") ]

                    -- TODO: Replace with translation
                    , div [ classes [ TC.w_100 ] ]
                        -- GRID!
                        [ div [ classes [ TC.mt4, TC.cf, TC.ph2_ns ] ]
                            -- First Row (First name, Last Name)
                            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ]
                              -- First element
                              <|
                                inputElement "First name" "First name" "text" FirstName model.firstName model.errors
                            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
                              -- Second element
                              <|
                                inputElement "Last name" "Last name" "text" LastName model.lastName model.errors
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Second Row (Subject, Semester number)
                            [ div [ classes [ TC.fl, TC.w_100, TC.w_70_ns ] ]
                              -- First element
                              <|
                                inputElement "Subject" "Subject" "text" Subject model.subject model.errors
                            , div [ classes [ TC.fl, TC.w_100, TC.w_30_ns, TC.pl2_ns ] ]
                              -- Second element
                              <|
                                inputElement "Semester" "1" "number" Semester model.semester model.errors
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Thrid Row (Student Number)
                            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                                inputElement "Student Number" "123456" "number" StudentNumber model.studentNumber model.errors
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Fourth Row (Email)
                            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                                inputElement "Email address" "Email" "email" Email model.email model.errors
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Fifth Row (Password, Password)
                            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ]
                              -- First element
                              <|
                                inputElement "Password" "Password" "password" Password model.password model.errors
                            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
                              -- Second element
                              <|
                                inputElement "Repeat Password" "Password" "password" PasswordRepeat model.passwordRepeat model.errors
                            ]
                        ]
                    , button
                        [ Styles.buttonGreyStyle
                        , classes [ TC.mt4, TC.w_100 ]
                        ]
                        [ text "Registrieren" ]

                    -- TODO: Replace with translation
                    ]
                , div [ classes [ TC.mt3 ] ]
                    [ button [ onClick <| NavigateTo LoginRoute, Styles.linkGreyStyle ] [ text "Anmelden" ] --TODO: Replace with translation
                    ]
                ]
            ]
        ]


noUniEmailDialog : SharedState -> Model -> Html Msg
noUniEmailDialog sharedState model =
    Dialog.modalDialog div
        [ Styles.dialogOverlayStyle
        ]
        (Dialog.dialog div
            [ Styles.dialogContainerStyle
            ]
            [ div
                [ classes [ TC.w_100, TC.ph1, TC.bb, TC.bw2, TC.b__black ] ]
                [ h1 [] [ text "No University E-Mail" ] ]
            , div
                [ classes [ TC.w_100, TC.mt4 ] ]
                [ p [ Styles.textStyle ] [ text "The provided E-Mail is no university E-Mail address. We can not send you any E-Mails. This includes the confirmation E-Mail. To confirm your E-Mail and use the course system you need to ask a tutor." ]
                , div [ classes [ TC.fr, TC.mt3 ] ]
                    [ button
                        [ classes
                            []
                        , Styles.buttonRedStyle
                        , onClick <| Register True
                        ]
                        [ text "Register anyway" ]
                    , button
                        [ classes
                            [ TC.ml3 ]
                        , Styles.buttonGreenStyle
                        , onClick <| NoUniversityMailWarningVisible False
                        ]
                        [ text "Change E-Mail" ]
                    ]
                ]
            ]
        )
        model.noUniversityEmailDialogState
        noUniversityMailDialogConfig


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
    = Email
    | Password
    | PasswordRepeat
    | FirstName
    | LastName
    | StudentNumber
    | Semester
    | Subject


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Email ->
            { model | email = value }

        Password ->
            { model | password = value }

        PasswordRepeat ->
            { model | passwordRepeat = value }

        FirstName ->
            { model | firstName = value }

        LastName ->
            { model | lastName = value }

        StudentNumber ->
            { model | studentNumber = value }

        Semester ->
            { model | semester = value }

        Subject ->
            { model | subject = value }


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, "Bitte gib deine E-Mail ein." )
            , ifInvalidEmail .email (\value -> ( Email, "Die eingegebene E-Mail Addresse " ++ value ++ " ist nicht gültig." ))
            ]
        , Validate.firstError
            [ ifBlank .semester ( Semester, "Bitte gib dein Semester ein." )
            , ifNotInt .semester (\value -> ( Semester, value ++ " ist keine gültige Zahl." ))
            , Validate.ifTrue (\model -> isZero model.semester ) ( Semester, "Auch als Informatiker beginnt das erste Semester mit 1 :).")
            , Validate.ifTrue (\model -> isNegative model.semester ) ( Semester, "Es gibt keine negativen Semester.")  
            ]
        , Validate.firstError
            [ ifBlank .password ( Password, "Bitte gib ein Passwort ein." ) -- TODO: Check if password is at least 7 characters long
            , ifBlank .passwordRepeat ( PasswordRepeat, "Bitte gib dein Passwort erneut ein." )
            , Validate.ifFalse (\model -> model.password == model.passwordRepeat) ( Password, "Die Passwörter müssen identisch sein." )
            ]
        , ifBlank .firstName ( FirstName, "Bitte gib deinen Vornamen ein." )
        , ifBlank .lastName ( LastName, "Bitte gib deinen Nachnamen ein." )
        , Validate.firstError
            [ ifBlank .studentNumber ( StudentNumber, "Bitte gib deine Martrikelnummer ein." )
            , ifNotInt .studentNumber (\value -> ( StudentNumber, value ++ " ist keine gültige Zahl." ))
            , Validate.ifTrue (\model -> isNegative model.studentNumber) (StudentNumber, "Matrikelnummern sind positiv.")
            ]
        , ifBlank .subject ( Subject, "Bitte gib dein Fach ein." )
        ]

isNegative : String -> Bool
isNegative numberString =
    case String.toInt numberString of
        Just num -> num < 0
        Nothing -> True

isZero : String -> Bool
isZero numberString =
    case String.toInt numberString of
        Just num -> num == 0
        Nothing -> True

addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )


noUniversityMailDialogConfig : Dialog.Config Msg
noUniversityMailDialogConfig =
    Dialog.Config
        Styles.dialogVisibleStyle
        Styles.dialogGoneStyle
        NoUniversityMailWarningVisible
        True
        NoOp
