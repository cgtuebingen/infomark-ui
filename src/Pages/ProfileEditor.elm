{-
   Change profile settings
-}


module Pages.ProfileEditor exposing (Model, Msg(..), init, update, view)

import Api.Data.Account exposing (Account)
import Api.Data.AccountUpdate exposing (AccountUpdate)
import Api.Data.User exposing (User)
import Api.Request.Account as AccountRequests
import Api.Request.Me as MeRequests
import Api.Request.User as UserRequests
import Browser.Navigation exposing (pushUrl)
import Components.Dialog as Dialog
import Components.Toasty
import Dict
import Dict.Extra exposing (groupBy)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, preventDefaultOn)
import Http
import I18n
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Task
import Time
import Toasty
import Types
import Utils.Styles as Styles
import Utils.Utils exposing (handleLogoutErrors)
import Validate exposing (Validator, ifBlank, ifInvalidEmail, ifNotInt, validate)


type Msg
    = UserGetResponse (WebData User)
    | SetField Field String
    | GotFiles File (List File)
    | GotPreview String
    | Pick
    | DragEnter
    | DragLeave
    | Save
    | RequestAccountDelete
    | PerformAccountDelete
    | AccountUpdateResponse (WebData ())
    | UserUpdateResponse (WebData ())
    | AvatarUpdateResponse (WebData ())
    | NavigateTo Route
    | AccountDeleteDialogShown Bool
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | NoOp


type alias Model =
    { user : WebData User
    , firstname : String
    , lastname : String
    , studentNumber : String
    , semester : String
    , subject : String
    , email : String
    , password : String
    , passwordRepeat : String
    , oldPassword : String
    , avatar : Maybe File
    , accountDataChanged : Bool
    , avatarChanged : Bool
    , userDataChanged : Bool
    , userErrors : List Error
    , accountErrors : List Error
    , hover : Bool
    , preview : String
    , toasties : Toasty.Stack Components.Toasty.Toast
    , accountDeleteDialog : Dialog.State
    }


init : ( Model, Cmd Msg )
init =
    ( { user = Loading
      , firstname = ""
      , lastname = ""
      , studentNumber = ""
      , semester = ""
      , subject = ""
      , email = ""
      , password = ""
      , passwordRepeat = ""
      , oldPassword = ""
      , avatar = Nothing
      , accountDataChanged = False
      , avatarChanged = False
      , userDataChanged = False
      , userErrors = []
      , accountErrors = []
      , hover = False
      , preview = ""
      , toasties = Toasty.initialState
      , accountDeleteDialog = False
      }
    , MeRequests.meGet UserGetResponse
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UserGetResponse response ->
            updateAccountGetResponse sharedState model response

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        Save ->
            updateSave sharedState model

        RequestAccountDelete ->
            ( model, Cmd.none, NoUpdate )

        PerformAccountDelete ->
            ( model, Cmd.none, NoUpdate )

        Pick ->
            ( model, Select.files [ "image/*" ] GotFiles, NoUpdate )

        DragEnter ->
            ( { model | hover = True }, Cmd.none, NoUpdate )

        DragLeave ->
            ( { model | hover = False }, Cmd.none, NoUpdate )

        GotFiles file files ->
            ( { model | hover = False, avatar = Just file, avatarChanged = True }
            , Task.perform GotPreview <| File.toUrl file
            , NoUpdate
            )

        GotPreview urls ->
            ( { model | preview = urls }
            , Cmd.none
            , NoUpdate
            )

        AccountUpdateResponse response ->
            updateAccountUpdateResponse sharedState model response

        UserUpdateResponse response ->
            updateUserUpdateResponse sharedState model response

        AvatarUpdateResponse response ->
            updateAvatarUpdateResponse sharedState model response

        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        AccountDeleteDialogShown visible ->
            ( { model | accountDeleteDialog = visible }, Cmd.none, NoUpdate )

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


updateAccountGetResponse : SharedState -> Model -> WebData User -> ( Model, Cmd Msg, SharedStateUpdate )
updateAccountGetResponse sharedState model response =
    case response of
        Success user ->
            let
                newModel =
                    userToModel model user
            in
            ( { newModel | user = response }, Cmd.none, NoUpdate )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    ( { model | user = response }, Cmd.none, NoUpdate )
                )
                err

        _ ->
            ( { model | user = response }, Cmd.none, NoUpdate )


updateSave : SharedState -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
updateSave sharedState model =
    let
        cmds =
            [ { changed = model.accountDataChanged
              , updateModel =
                    \m ->
                        { m
                            | accountErrors =
                                case validate accountValidator m of
                                    Err err ->
                                        err

                                    Ok _ ->
                                        []
                        }
              , request = AccountRequests.accountPatch (modelToAccount model) AccountUpdateResponse
              }
            , { changed = model.userDataChanged
              , updateModel =
                    \m ->
                        { m
                            | userErrors =
                                case validate userValidator m of
                                    Err err ->
                                        err

                                    Ok _ ->
                                        []
                        }
              , request = userUpdateRequest sharedState model UserUpdateResponse
              }
            , { changed = model.avatarChanged
              , updateModel = \m -> m
              , request = avatarUpdateRequest model AvatarUpdateResponse
              }
            ]

        changed =
            List.filter .changed cmds

        -- All changed parts: User, Account, Avatar
        updatedModel =
            List.foldl .updateModel model changed

        requests =
            List.map .request changed
    in
    case ( updatedModel.userErrors, updatedModel.accountErrors ) of
        ( [], [] ) ->
            -- Ready to update
            ( updatedModel, Cmd.batch requests, NoUpdate )

        ( _, _ ) ->
            -- There are errors
            ( updatedModel, Cmd.none, NoUpdate )


updateUserUpdateResponse : SharedState -> Model -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateUserUpdateResponse sharedState model response =
    case response of
        Success _ ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> addToast (Components.Toasty.Success "Success" "Profile updated!")
            in
            ( { newModel | userDataChanged = False }, newCmd, NoUpdate )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" "Failed to update profile!")
                    in
                    ( newModel, newCmd, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


updateAccountUpdateResponse : SharedState -> Model -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateAccountUpdateResponse sharedState model response =
    case response of
        Success _ ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> addToast (Components.Toasty.Success "Success" "Profile updated!")
            in
            ( { newModel | accountDataChanged = False }, newCmd, NoUpdate )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" "Failed to update profile!")
                    in
                    ( newModel, newCmd, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


updateAvatarUpdateResponse : SharedState -> Model -> WebData () -> ( Model, Cmd Msg, SharedStateUpdate )
updateAvatarUpdateResponse sharedState model response =
    case response of
        Success _ ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> addToast (Components.Toasty.Success "Success" "Profile updated!")
            in
            ( { newModel | avatarChanged = False }, newCmd, NoUpdate )

        Failure err ->
            handleLogoutErrors model
                sharedState
                (\e ->
                    let
                        ( newModel, newCmd ) =
                            ( model, Cmd.none )
                                |> addToast (Components.Toasty.Error "Error" "Failed to update profile!")
                    in
                    ( newModel, newCmd, NoUpdate )
                )
                err

        _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    div [ classes [ TC.db, TC.pv5_l, TC.pv3_m, TC.pv1, TC.ph0_ns, TC.w_100 ] ]
        [ Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
        , div
            [ classes
                [ TC.mw8
                , TC.ph4
                , TC.ph5_ns
                , TC.center
                ]
            ]
            [ viewFormLoadingOrError sharedState model ]
        ]


viewFormLoadingOrError : SharedState -> Model -> Html Msg
viewFormLoadingOrError sharedState model =
    case model.user of
        Loading ->
            -- Display spinner
            div [] []

        Failure (Http.BadStatus 403) ->
            text "Not permitted"

        _ ->
            -- In all other cases display the form
            viewForm sharedState model


viewForm : SharedState -> Model -> Html Msg
viewForm sharedState model =
    div
        [ classes [ TC.w_100 ] ]
        [ h1
            [ Styles.headerStyle

            --, classes [TC.bl_0, TC.br_0, TC.bt_0, TC.bb, TC.bw2, TC.b__black]
            ]
            [ text "Profil bearbeiten" ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            -- First Rows (Avatar uploader & Name)
            [ avatarUploader model
            , div
                [ classes
                    [ TC.fl
                    , TC.w_100
                    , TC.w_50_ns
                    , TC.pl4_ns
                    , TC.mt3_ns
                    , TC.mt4
                    ]
                ]
              <|
                (inputElement "First Name" "First Name" "text" FirstName model.firstname model.userErrors
                    ++ inputElement "Last Name" "Last Name" "text" LastName model.lastname model.userErrors
                )
            ]
        , div [ classes [ TC.mt3, TC.mt4_ns, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100, TC.w_70_ns ] ] <|
                inputElement "Subject" "Subject" "text" Subject model.subject model.userErrors
            , div [ classes [ TC.fl, TC.w_100, TC.w_30_ns, TC.pl2_ns ] ] <|
                inputElement "Semester" "1" "number" Semester model.semester model.userErrors
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                inputElement "Student Number" "123456" "number" StudentNumber model.studentNumber model.userErrors
            ]
        , h2
            [ Styles.sectionStyle
            , classes [ TC.mb3, TC.mt0 ]
            ]
            [ text "Account" ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                inputElement "Email" "Email" "email" Email model.email model.accountErrors
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ] <|
                inputElement "New Password" "Password" "password" Password model.password model.accountErrors
            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ] <|
                inputElement "New Password Repeat" "Password" "password" PasswordRepeat model.passwordRepeat model.accountErrors
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                inputElement "Old Password" "Password" "password" OldPassword model.oldPassword model.accountErrors
            ]
        , div [ classes [ TC.mt3, TC.cf, TC.ph4_ns, TC.ph3 ] ]
            [ button
                ([ classes [ TC.w_100 ]
                 ]
                    ++ (if model.accountDataChanged || model.userDataChanged || model.avatarChanged then
                            [ Styles.buttonGreenStyle
                            , onClick Save
                            ]

                        else
                            [ Styles.buttonDisabled ]
                       )
                )
                [ text "Save" ]
            ]
        , h2
            [ Styles.sectionStyle
            , classes [ TC.mt5, TC.bt, TC.b__dark_gray, TC.bw2, TC.pt4 ]
            ]
            [ text "Account Actions" ]
        , div [ classes [ TC.mt3, TC.ph2_ns, TC.flex, TC.justify_center, TC.items_center, TC.flex_wrap ] ]
            [ p [ classes [ TC.w_100, TC.w_50_ns ], Styles.textStyle ]
                [ text "Delete your account? This cannot be undone and you lose every submission and contribution to all courses." ]
            , div [ classes [ TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
                [ button
                    [ Styles.buttonRedStyle
                    , classes [ TC.w_100 ]
                    , onClick RequestAccountDelete
                    ]
                    [ text "Delete" ]
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


avatarUploader : Model -> Html Msg
avatarUploader model =
    div
        [ classes
            [ TC.pa4
            , TC.ba
            , TC.b__dashed
            , if model.hover then
                TC.b__dark_red

              else
                TC.b__black_40
            , TC.bw2
            , TC.br3
            , TC.w_50_ns
            , TC.w_100
            , TC.flex
            , TC.flex_column
            , TC.justify_center
            , TC.items_center
            , TC.fl
            ]
        , hijackOn "dragenter" (Decode.succeed DragEnter)
        , hijackOn "dragover" (Decode.succeed DragEnter)
        , hijackOn "dragleave" (Decode.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ div
            [ classes
                [ TC.w4
                , TC.h4
                , TC.contain
                , TC.bg_center
                ]
            , style "background-image" ("url('" ++ model.preview ++ "')")
            ]
            []
        , button
            [ Styles.buttonGreyStyle
            , classes
                [ TC.w_100
                , TC.mt4
                ]
            , onClick Pick
            ]
            [ text "Pick avatar" ]
        ]


viewFormErrors : Field -> List Error -> Html Msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [ classes [ TC.red ] ] [ text error ])
        |> ul [ classes [ TC.list, TC.pl0, TC.center ] ]


type Field
    = FirstName
    | LastName
    | Email
    | StudentNumber
    | Semester
    | Subject
    | OldPassword
    | Password
    | PasswordRepeat


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        FirstName ->
            { model | firstname = value, userDataChanged = True }

        LastName ->
            { model | lastname = value, userDataChanged = True }

        Email ->
            { model | email = value, accountDataChanged = True }

        StudentNumber ->
            { model | studentNumber = value, userDataChanged = True }

        Semester ->
            { model | semester = value, userDataChanged = True }

        Subject ->
            { model | subject = value, userDataChanged = True }

        Password ->
            { model | password = value, accountDataChanged = True }

        PasswordRepeat ->
            { model | passwordRepeat = value, accountDataChanged = True }

        OldPassword ->
            { model | oldPassword = value }


type alias Error =
    ( Field, String )


modelToUser : SharedState -> Model -> Maybe User
modelToUser sharedState model =
    case model.user of
        Success user ->
            Just
                { id = user.id
                , firstname = model.firstname
                , lastname = model.lastname
                , avatarUrl = Nothing
                , email = model.email
                , studentNumber = Just model.studentNumber
                , semester = String.toInt model.semester
                , subject = Just model.subject
                , language = Just <| Types.languageToBackendString sharedState.selectedLanguage
                }

        _ ->
            Nothing


modelToAccount : Model -> AccountUpdate
modelToAccount model =
    { account =
        { email =
            if model.email == "" then
                Nothing

            else
                Just model.email
        , plain_password =
            if model.password == "" then
                Nothing

            else
                Just model.password
        }
    , oldPassword = model.oldPassword
    }


userToModel : Model -> User -> Model
userToModel model user =
    { model
        | firstname = user.firstname
        , lastname = user.lastname
        , studentNumber = Maybe.withDefault "" user.studentNumber
        , semester = Maybe.withDefault "" <| Maybe.map String.fromInt user.semester
        , subject = Maybe.withDefault "" user.subject
        , email = user.email
        , preview =
            case user.avatarUrl of
                Just url ->
                    url

                Nothing ->
                    "assets/defaultAvatar.png"
    }


userValidator : Validator Error Model
userValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .semester ( Semester, "Bitte gib dein Semester ein." )
            , ifNotInt .semester (\value -> ( Semester, value ++ " ist keine gültige Zahl." ))
            , Validate.ifTrue (\model -> isZero model.semester) ( Semester, "Auch als Informatiker beginnt das erste Semester mit 1 :)." )
            , Validate.ifTrue (\model -> isNegative model.semester) ( Semester, "Es gibt keine negativen Semester." )
            ]
        , ifBlank .firstname ( FirstName, "Bitte gib deinen Vornamen ein." )
        , ifBlank .lastname ( LastName, "Bitte gib deinen Nachnamen ein." )
        , Validate.firstError
            [ ifBlank .studentNumber ( StudentNumber, "Bitte gib deine Martrikelnummer ein." )
            , ifNotInt .studentNumber (\value -> ( StudentNumber, value ++ " ist keine gültige Zahl." ))
            , Validate.ifTrue (\model -> isNegative model.studentNumber) ( StudentNumber, "Matrikelnummern sind positiv." )
            ]
        , ifBlank .subject ( Subject, "Bitte gib dein Fach ein." )
        ]


accountValidator : Validator Error Model
accountValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, "Bitte gib deine E-Mail ein." )
            , ifInvalidEmail .email (\value -> ( Email, "Die eingegebene E-Mail Addresse " ++ value ++ " ist nicht gültig." ))
            ]
        , Validate.firstError
            [ Validate.ifFalse (\model -> model.password == model.passwordRepeat) ( Password, "Die Passwörter müssen identisch sein." )
            , Validate.ifTrue (\model -> String.length model.password < 7 && String.length model.password > 0) ( Password, "Das Passwort muss mindestens 7 Zeichen lang sein." )
            ]
        , Validate.ifBlank .oldPassword ( OldPassword, "Bitte gib dein altes Passwort ein." )
        ]


isNegative : String -> Bool
isNegative numberString =
    case String.toInt numberString of
        Just num ->
            num < 0

        Nothing ->
            True


isZero : String -> Bool
isZero numberString =
    case String.toInt numberString of
        Just num ->
            num == 0

        Nothing ->
            True


userUpdateRequest : SharedState -> Model -> (WebData () -> msg) -> Cmd msg
userUpdateRequest sharedState model msg =
    case ( model.user, modelToUser sharedState model ) of
        ( Success user, Just userUpdate ) ->
            MeRequests.mePut userUpdate msg

        ( _, _ ) ->
            Cmd.none


avatarUpdateRequest : Model -> (WebData () -> msg) -> Cmd msg
avatarUpdateRequest model msg =
    case model.avatar of
        Just file ->
            AccountRequests.accountAvatarPost file msg

        Nothing ->
            Cmd.none


dropDecoder : Decoder Msg
dropDecoder =
    Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )
