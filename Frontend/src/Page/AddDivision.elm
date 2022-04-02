module Page.AddDivision exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)



-- Types --


type alias Model =
    {}


type Msg
    = None



-- Init --


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



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
    text "skeleton"
