module Components.Dialog exposing (State, Config, modalDialog, dialog)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, stopPropagationOn, onClick)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode


{-| Indicates whether the dialog is visible or not
-}
type alias State =
    Bool

{-| The config for the modal. The dialogVisibleAttribute and dialogHiddenAttribute
are automatically applied to the dialog based on the current state.

The callback is the current visible callback. If the dialog is cancelable it is
called automatically and closes the dialog. Cancelable means a click on the overlay
disabled the dialog. Due to https://github.com/elm/html/issues/137 we need to pass
a message when we want to stop propagating clicks to the overlay from the dialog. 
Therefore, you need to pass a noOp msg.
-}
type alias Config msg =
    { dialogVisibleAttribute : Html.Attribute msg
    , dialogHiddenAttribute : Html.Attribute msg
    , callback : Bool -> msg
    , cancelable : Bool
    , noOp : msg 
    }

{-! Represents the whole modal dialog. You need to add a dialog
element.

    cancelable = True -- Defines whether clicks on the background cancel the dialog

    modalDialog cancelable div --ShowDialog Bool needs to be added here
        [ class "dialog_background" ]
        dialog div [ class "dialog_container" ] 
            [ p [] [text "This is quite a nice Dialog"]
            , button [] [text "Confirm"]
            , button [ onClick <| ShowDialog False] [text "Nope"]
            ] 
        model.dialogState
        dialogConfig
-}

modalDialog : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> (State -> Config msg -> Html msg) -> State -> Config msg -> Html msg
modalDialog element attributes child isVisible config =
    let
        clickEvent = if config.cancelable then
                [ custom "click"
                    (Decode.succeed
                        { message = config.callback (not isVisible)
                        , preventDefault = True
                        , stopPropagation = True
                        }
                    )
                ]
            else
                []

        visiblity = if isVisible then
                [config.dialogVisibleAttribute]
            else
                [config.dialogHiddenAttribute]
    in
    element 
        (visiblity ++ clickEvent ++ attributes)  -- TODO check if we need more attributes
        [(child isVisible config)]


{-| Transform the given HTML-elements into the actual dialog.

Example of use

    dialog div
        [ class "dialogStyle" ]
        [ p [] [text "This is my dialog!"] 
        , button [ onClick Confirm ] [ text "Confirm" ]
        , button [ onClick Cancel ] [ text "Cancel" ]
        ]
-}
dialog : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> List (Html msg) -> State -> Config msg -> Html msg
dialog element givenAttributes children isVisible config =
    let
        clickEvent = custom "click"
            (Decode.succeed
                { message = config.noOp
                , preventDefault = True
                , stopPropagation = True
                }
            )
    in
    element
        ( clickEvent :: givenAttributes )
        children