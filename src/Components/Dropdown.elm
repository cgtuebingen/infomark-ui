module Components.Dropdown exposing
    ( State, Config, ToggleEvent(..)
    , dropdown, toggle, drawer
    )

{-| Flexible dropdown component which serves as a foundation for custom dropdowns, select–inputs, popovers, and more.


# Example

Basic example of usage:

    init : Model
    init =
        { myDropdown = False }

    type alias Model =
        { myDropdown : Dropdown.State }

    type Msg
        = ToggleDropdown Bool

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ToggleDropdown newState ->
                ( { model | myDropdown = newState }, Cmd.none )

    view : Model -> Html Msg
    view model =
        div []
            [ dropdown
                model.myDropdown
                myDropdownConfig
                (toggle button [] [ text "Toggle" ])
                (drawer div
                    []
                    [ button [] [ text "Option 1" ]
                    , button [] [ text "Option 2" ]
                    , button [] [ text "Option 3" ]
                    ]
                )
            ]

    myDropdownConfig : Dropdown.Config Msg
    myDropdownConfig =
        Dropdown.Config
            "myDropdown"
            OnClick
            (class "visible")
            ToggleDropdown


# Configuration

@docs State, Config, ToggleEvent


# Views

@docs dropdown, toggle, drawer

-}

{-
   Taken from https://github.com/circuithub/elm-dropdown/blob/master/src/Dropdown.elm
-}

import Html exposing (Html)
import Html.Attributes exposing (property, style, tabindex)
import Html.Events exposing (custom, keyCode, on, onFocus, onMouseEnter)
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE


{-| Indicates wether the dropdown's drawer is visible or not.
-}
type alias State =
    Bool


{-| Configuration.

  - `identifier`: unique identifier for the dropdown.
  - `toggleEvent`: Event on which the dropdown's drawer should appear or disappear.
  - `drawerVisibleAttribute`: `Html.Attribute msg` that's applied to the dropdown's drawer when visible.
  - `callback`: msg which will be called when the state of the dropdown should be changed.

-}
type alias Config msg =
    { identifier : String
    , toggleEvent : ToggleEvent
    , drawerVisibleAttribute : Html.Attribute msg
    , callback : Bool -> msg
    }


{-| Used to set the event on which the dropdown's drawer should appear or disappear.
-}
type ToggleEvent
    = OnClick
    | OnHover
    | OnFocus


{-| Creates a dropdown using the given state, config, toggle, and drawer.

    dropdown div
        []
        [ toggle button
            [ class "myButton" ]
            [ text "More options" ]
        , drawer div
            [ class "myDropdownDrawer" ]
            [ button [ onClick NewFile ] [ text "New" ]
            , button [ onClick OpenFile ] [ text "Open..." ]
            , button [ onClick SaveFile ] [ text "Save" ]
            ]
        ]
        model.myDropdownState
        myDropdownConfig

-}
dropdown : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> List (State -> Config msg -> Html msg) -> State -> Config msg -> Html msg
dropdown element attributes children isOpen config =
    let
        toggleEvents =
            case config.toggleEvent of
                OnHover ->
                    [ on "mouseout" (handleFocusChanged isOpen config)
                    , on "focusout" (handleFocusChanged isOpen config)
                    ]

                _ ->
                    [ on "focusout" (handleFocusChanged isOpen config) ]
    in
    element
        ( (on "keydown" (handleKeyDown isOpen config) )
            :: toggleEvents
            ++ [ anchor config.identifier
               , tabindex -1
               , pRelative
               , dInlineBlock
               , outlineNone
               ]
            ++ attributes
        )
        (List.map (\child -> child isOpen config) children)


{-| Transforms the given HTML-element into a working toggle for your dropdown.
See `dropdown` on how to use in combination with `drawer`.

Example of use:

    toggle button
        [ class "myButton" ]
        [ text "More options" ]

-}
toggle : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> List (Html msg) -> State -> Config msg -> Html msg
toggle element attributes children isOpen model =
    let
        toggleEvents =
            case model.toggleEvent of
                OnClick ->
                    [ custom "click"
                        (JD.succeed
                            { message = model.callback (not isOpen)
                            , preventDefault = True
                            , stopPropagation = True
                            }
                        )
                    ]

                OnHover ->
                    [ onMouseEnter (model.callback True)
                    , onFocus (model.callback True)
                    ]

                OnFocus ->
                    [ onFocus (model.callback True) ]
    in
    element
        (toggleEvents ++ attributes)
        children


{-| Transforms the given HTML-element into a working drawer for your dropdown.
See `dropdown` on how to use in combination with `toggle`.

Example of use:

    drawer div
        [ class "myDropdownDrawer" ]
        [ button [ onClick NewFile ] [ text "New" ]
        , button [ onClick OpenFile ] [ text "Open..." ]
        , button [ onClick SaveFile ] [ text "Save" ]
        ]

-}
drawer : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> List (Html msg) -> State -> Config msg -> Html msg
drawer element givenAttributes children isOpen config =
    let
        attributes =
            if isOpen then
                config.drawerVisibleAttribute :: [ vVisible, pAbsolute ] ++ givenAttributes

            else
                [ vHidden, pAbsolute ] ++ givenAttributes
    in
    element
        attributes
        children


anchor : String -> Html.Attribute msg
anchor identifier =
    property "dropdownId" (JE.string identifier)


handleKeyDown : State -> Config msg -> JD.Decoder msg
handleKeyDown isOpen { identifier, callback } =
    JD.map callback
        (keyCode
            |> JD.andThen
                (JD.succeed << (&&) isOpen << not << (==) 27)
        )


handleFocusChanged : State -> Config msg -> JD.Decoder msg
handleFocusChanged isOpen { identifier, callback } =
    JD.map callback (isFocusOnSelf identifier)


isFocusOnSelf : String -> JD.Decoder Bool
isFocusOnSelf identifier =
    JD.field "relatedTarget" (decodeDomElement identifier)
        |> JD.andThen isChildOfSelf
        |> JD.withDefault False


decodeDomElement : String -> JD.Decoder DomElement
decodeDomElement identifier =
    JD.map2 DomElement
        (JD.field "dropdownId" JD.string |> JD.andThen (isDropdown identifier) |> JD.withDefault False)
        (JD.field "parentElement" (JD.map ParentElement (JD.lazy (\_ -> decodeDomElement identifier)) |> JD.maybe))


isDropdown : String -> String -> JD.Decoder Bool
isDropdown identifier identifier2 =
    JD.succeed (identifier == identifier2)


isChildOfSelf : DomElement -> JD.Decoder Bool
isChildOfSelf cfg =
    if cfg.isDropdown then
        JD.succeed True

    else
        case cfg.parentElement of
            Nothing ->
                JD.succeed False

            Just (ParentElement domElement) ->
                isChildOfSelf domElement


type alias DomElement =
    { isDropdown : Bool
    , parentElement : Maybe ParentElement
    }


type ParentElement
    = ParentElement DomElement


vVisible : Html.Attribute msg
vVisible =
    style "visibility" "visible"


vHidden : Html.Attribute msg
vHidden =
    style "visibility" "hidden"


pRelative : Html.Attribute msg
pRelative =
    style "position" "relative"


pAbsolute : Html.Attribute msg
pAbsolute =
    style "position" "absolute"


dInlineBlock : Html.Attribute msg
dInlineBlock =
    style "display" "inline-block"


outlineNone : Html.Attribute msg
outlineNone =
    style "outline" "none"
