module Page.AddGame exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Session exposing (Session)



-- Types --


type alias Model =
    { session : Session
    }


type Msg
    = None



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- API Requests --
-- View --


view : Model -> Html Msg
view _ =
    text "add game"
