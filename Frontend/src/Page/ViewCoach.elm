module Page.ViewCoach exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Session exposing (Session)
import Model.SharedIds exposing (CoachId)



-- Types --


type alias Model =
    { session : Session
    }


type Msg
    = None



-- Init --


init : Session -> CoachId -> ( Model, Cmd Msg )
init session id =
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
    text "skeleton"
