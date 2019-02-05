module Pages.Login exposing(Model, Msg, initModel, update)

import Browser.Navigation exposing (pushUrl)
import Decoders
import Dict
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Time
import Types exposing (Language(..), Translations)


type alias Model =
    { username : String
    , plain_password : String
    }

type Msg
    = NavigateTo Route


initModel : Model
initModel =
    { username = ""
    , plain_password = ""
    }

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate)
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

