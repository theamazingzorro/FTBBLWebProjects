module Page.EditCoach exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Coach exposing (CoachId, idToString)



-- Types --


type alias Model =
    { id : CoachId
    }


type Msg
    = None



-- Init --


init : CoachId -> ( Model, Cmd Msg )
init id =
    ( { id = id }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- View --


view : Model -> Html Msg
view model =
    div [] [ text <| idToString model.id ]
