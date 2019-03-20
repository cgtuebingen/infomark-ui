module Components.UserAvatarEmailView exposing 
    ( Model
    , initFromUser
    , updateFromUserAvatar
    , view
    )
{-| Convenience view to display a user with avatar and add
    a link to the email view
-}


import Api.Data.User as User exposing (User)
import Browser.Navigation exposing (pushUrl)
import Routing.Helpers exposing (Route(..), reverseRoute)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import SharedState exposing (SharedState)
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Utils.Styles as Styles


type alias Model =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    , avatarUrl : String
    }


initFromUser : User -> (Model, Cmd msg)
initFromUser user =
    (
        { id = user.id 
        , firstName = user.firstname
        , lastName = user.lastname
        , email = user.email
        , avatarUrl = case user.avatarUrl of
                Just avatarUrl ->
                    avatarUrl

                Nothing ->
                    "assets/defaultAvatar.png"
        }
    , Cmd.none
    )

updateFromUserAvatar : SharedState -> Int -> Cmd msg
updateFromUserAvatar sharedState userId =
    pushUrl sharedState.navKey (reverseRoute <| MailToUsersRoute userId)


view : SharedState -> Model -> (Int -> msg) -> Html msg
view sharedState model msg =
    div [ classes [ TC.flex, TC.items_center, TC.pa3, TC.ph0_l, TC.w_third_l, TC.w_50_m, TC.w_100 ] ]
        [ img
            [ src model.avatarUrl
            , classes
                [ TC.br_100
                , TC.ba
                , TC.b__black_10
                , TC.shadow_5_ns
                , TC.h3_ns
                , TC.h2
                , TC.w3_ns
                , TC.w2
                ]
            ]
            []
        , div [ classes [ TC.flex_auto, TC.pl2 ] ]
            [ h1 [ Styles.listHeadingStyle, classes [ TC.mv0, TC.ph2 ] ] [ text (model.firstName ++ " " ++ model.lastName) ]
            , button 
                [ Styles.linkGreyStyle
                , classes [ TC.mv0, TC.tl, TC.mh0 ]
                , style "word-break" "break-all"
                , onClick <| msg model.id
                ] [ text model.email ]
            ]
        ]