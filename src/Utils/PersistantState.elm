port module Utils.PersistantState exposing 
    ( State(..)
    , logout
    , sharedStateUpdateToStorage
    , storageToState
    , stateMsgToSharedStateUpdate
    , decode)

import Api.Data.Role as Role exposing (Role)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import SharedState exposing (SharedState, SharedStateUpdate(..))


type State
    = State Role String


stateDecoder : Decoder State
stateDecoder =
    Decode.succeed State
        |> required "role" Role.decoder
        |> required "email" Decode.string

    
stateEncoder : Role -> String -> Value
stateEncoder role mail =
    Encode.object
        [ ( "email", Encode.string mail )
        , ( "role", Role.encoder role ) 
        ]

--sharedStateToStorage : SharedState -> Cmd msg

logout : Cmd msg
logout =
    storeCache Nothing

{-sharedStateUpdateToStorage : (String -> Role -> sharedStateUpdate) -> Cmd msg
sharedStateUpdateToStorage toSharedStateUpdate =
    storeCache Just <| (\(mail, role) -> stateEncoder mail role)-}
sharedStateUpdateToStorage : SharedStateUpdate -> Cmd msg
sharedStateUpdateToStorage sharedStateUpdate =
    case sharedStateUpdate of
        UpdateRoleAndMail role mail -> storeCache <| Just <| stateEncoder role mail
        _ -> Cmd.none

storageToState : (Maybe State -> msg) -> Sub msg
storageToState toMsg =
    onStoreChange (\value -> 
        toMsg (decode value)
    )

stateMsgToSharedStateUpdate : Maybe State -> SharedStateUpdate
stateMsgToSharedStateUpdate maybeState =
    case maybeState of
        Just (State role mail) -> UpdateRoleAndMail role mail
        Nothing -> NoUpdate

port onStoreChange : (Value -> msg) -> Sub msg

port storeCache : Maybe Value -> Cmd msg

decode : Value -> Maybe State
decode value =
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString stateDecoder str) 
        |> Result.toMaybe