module Header exposing (Model, Msg, init, update, view)

import Html exposing (..)


type alias Model =
    {}


type Msg
    = None


init : ( Model, Cmd Msg )
init = 
    ( {}, Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None -> 
            ( model, Cmd.none )


view : Model -> Html Msg
view model = 
    div []
        [ text "test"]