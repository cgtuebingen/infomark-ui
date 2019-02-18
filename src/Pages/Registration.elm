module Pages.Registration exposing (Error, Field(..), Model, Msg(..), init, inputElement, modelValidator, setField, update, view, viewFormErrors)

import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Types exposing (Language(..), Translations)
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
    , registrationProgress : WebData String
    , errors : List Error
    }


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
            ]
        , ifBlank .subject ( Subject, "Bitte gib dein Fach ein." )
        ]


type Msg
    = NavigateTo Route
    | Register
    | SetField Field String
    | RegistrationResponse (WebData String)


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

        Register ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    ( { model | registrationProgress = Loading, errors = [] }, Cmd.none, NoUpdate )

        -- TODO: Start the web request here.
        RegistrationResponse response ->
            ( model, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )



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
        [ div
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
                , onSubmit Register
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
                                inputElement "Semester" "Semester" "number" Semester model.semester model.errors
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Thrid Row (Student Number)
                            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                                inputElement "Student Number" "Student Number" "number" StudentNumber model.studentNumber model.errors
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
