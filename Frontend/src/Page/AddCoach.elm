module Page.AddCoach exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Coach exposing (Coach)



-- Types --


type alias Model =
    { coach : Coach
    }


type Msg
    = T



-- Init --


init : ( Model, Cmd Msg )
init =
    ( { coach = { id = 0, name = "", elo = 1000 } }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        T ->
            ( model, Cmd.none )



-- View --


view : Model -> Html Msg
view _ =
    h3 [] [ text "Add Coach" ]
